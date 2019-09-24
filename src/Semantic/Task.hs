{-# LANGUAGE ConstraintKinds, ExistentialQuantification, GADTs, GeneralizedNewtypeDeriving, KindSignatures,
             ScopedTypeVariables, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Semantic.Task
( Task
, TaskEff
, Level(..)
, RAlgebra
-- * I/O
, Files.readBlob
, Files.readBlobs
, Files.readBlobPairs
, Files.readProject
, Files.findFiles
, Files.write
, Files.FilesArg(..)
-- * Module Resolution
, resolutionMap
, Resolution
-- * Telemetry
, writeLog
, writeStat
, time
, time'
-- * High-level flow
, parse
, decorate
, diff
, render
, serialize
-- * Concurrency
, distribute
, distributeFor
, distributeFoldMap
-- * Configuration
, debugOptions
, defaultOptions
, defaultConfig
, terminalFormatter
, logfmtFormatter
-- * Interpreting
, runTask
, runTaskWithOptions
, withOptions
, TaskSession(..)
, runTraceInTelemetry
, runTaskF
-- * Exceptions
, ParserCancelled(..)
-- * Re-exports
, Distribute
, Error
, Lift
, throwError
, SomeException(..)
, Telemetry
) where

import           Analysis.Decorator (decoratorWithAlgebra)
import qualified Assigning.Assignment as Assignment
import qualified Assigning.Assignment.Deterministic as Deterministic
import           Control.Effect.Carrier
import           Control.Effect.Catch
import           Control.Effect.Error
import           Control.Effect.Lift
import           Control.Effect.Reader
import           Control.Effect.Resource
import           Control.Effect.Trace
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Diff
import qualified Data.Error as Error
import qualified Data.Flag as Flag
import           Data.Sum
import qualified Data.Syntax as Syntax
import           Data.Term
import           Diffing.Algorithm (Diffable)
import           Diffing.Interpreter
import           Parsing.CMark
import           Parsing.Parser
import           Parsing.TreeSitter
import           Prologue hiding (project)
import           Semantic.Config
import           Semantic.Distribute
import           Semantic.Resolution
import qualified Semantic.Task.Files as Files
import           Semantic.Telemetry
import           Semantic.Timeout
import           Serializing.Format hiding (Options)
import           Source.Loc
import           Source.Source (Source)

-- | A high-level task producing some result, e.g. parsing, diffing, rendering. 'Task's can also specify explicit concurrency via 'distribute', 'distributeFor', and 'distributeFoldMap'
type TaskEff
  = TaskC
  ( ResolutionC
  ( Files.FilesC
  ( ReaderC TaskSession
  ( TraceInTelemetryC
  ( TelemetryC
  ( ErrorC SomeException
  ( TimeoutC
  ( ResourceC
  ( CatchC
  ( DistributeC
  ( LiftC IO)))))))))))

-- | A function to render terms or diffs.
type Renderer i o = i -> o

-- | A task which parses a 'Blob' with the given 'Parser'.
parse :: (Member Task sig, Carrier sig m)
      => Parser term
      -> Blob
      -> m term
parse parser blob = send (Parse parser blob pure)

-- | A task which decorates a 'Term' with values computed using the supplied 'RAlgebra' function.
decorate :: (Functor f, Member Task sig, Carrier sig m)
         => RAlgebra (TermF f Loc) (Term f Loc) field
         -> Term f Loc
         -> m (Term f field)
decorate algebra term = send (Decorate algebra term pure)

-- | A task which diffs a pair of terms using the supplied 'Differ' function.
diff :: (Diffable syntax, Eq1 syntax, Hashable1 syntax, Traversable syntax, Member Task sig, Carrier sig m)
     => These (Term syntax ann) (Term syntax ann)
     -> m (Diff syntax ann ann)
diff terms = send (Semantic.Task.Diff terms pure)

-- | A task which renders some input using the supplied 'Renderer' function.
render :: (Member Task sig, Carrier sig m)
       => Renderer input output
       -> input
       -> m output
render renderer input = send (Render renderer input pure)

serialize :: (Member Task sig, Carrier sig m)
          => Format input
          -> input
          -> m Builder
serialize format input = send (Serialize format input pure)

data TaskSession
  = TaskSession
  { config    :: Config
  , requestID :: String
  , isPublic  :: Bool
  , logger    :: LogQueue
  , statter   :: StatQueue
  }

-- | Execute a 'TaskEff' yielding its result value in 'IO'.
runTask :: TaskSession -> TaskEff a -> IO (Either SomeException a)
runTask taskSession@TaskSession{..} task = do
  (result, stat) <- withTiming "run" [] $ do
    let run :: TaskEff a -> IO (Either SomeException a)
        run
          = runM
          . withDistribute
          . runCatch
          . runResource
          . withTimeout
          . runError
          . runTelemetry logger statter
          . runTraceInTelemetry
          . runReader taskSession
          . Files.runFiles
          . runResolution
          . runTaskF
    run task
  queueStat statter stat
  pure result

-- | Execute a 'TaskEff' yielding its result value in 'IO' using all default options and configuration.
runTaskWithOptions :: Options -> TaskEff a -> IO (Either SomeException a)
runTaskWithOptions options task = withOptions options $ \ config logger statter ->
  runTask (TaskSession config "-" False logger statter) task

-- | Yield config and telemetry queues for options.
withOptions :: Options -> (Config -> LogQueue -> StatQueue -> IO a) -> IO a
withOptions options with = do
  config <- defaultConfig options
  withTelemetry config (\ (TelemetryQueues logger statter _) -> with config logger statter)

runTraceInTelemetry :: TraceInTelemetryC m a
                    -> m a
runTraceInTelemetry = runTraceInTelemetryC

newtype TraceInTelemetryC m a = TraceInTelemetryC { runTraceInTelemetryC :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Member Telemetry sig, Carrier sig m) => Carrier (Trace :+: sig) (TraceInTelemetryC m) where
  eff (R other)         = TraceInTelemetryC . eff . handleCoercible $ other
  eff (L (Trace str k)) = writeLog Debug str [] >> k


-- | An effect describing high-level tasks to be performed.
data Task (m :: * -> *) k
  = forall term . Parse (Parser term) Blob (term -> m k)
  | forall f field . Functor f => Decorate (RAlgebra (TermF f Loc) (Term f Loc) field) (Term f Loc) (Term f field -> m k)
  | forall syntax ann . (Diffable syntax, Eq1 syntax, Hashable1 syntax, Traversable syntax) => Diff (These (Term syntax ann) (Term syntax ann)) (Diff syntax ann ann -> m k)
  | forall input output . Render (Renderer input output) input (output -> m k)
  | forall input . Serialize (Format input) input (Builder -> m k)

deriving instance Functor m => Functor (Task m)

instance HFunctor Task where
  hmap f (Parse parser blob k)        = Parse parser blob (f . k)
  hmap f (Decorate decorator term k)  = Decorate decorator term (f . k)
  hmap f (Semantic.Task.Diff terms k) = Semantic.Task.Diff terms (f . k)
  hmap f (Render renderer input k)    = Render renderer input (f . k)
  hmap f (Serialize format input k)   = Serialize format input (f . k)

instance Effect Task where
  handle state handler (Parse parser blob k)        = Parse parser blob (handler . (<$ state) . k)
  handle state handler (Decorate decorator term k)  = Decorate decorator term (handler . (<$ state) . k)
  handle state handler (Semantic.Task.Diff terms k) = Semantic.Task.Diff terms (handler . (<$ state) . k)
  handle state handler (Render renderer input k)    = Render renderer input (handler . (<$ state) . k)
  handle state handler (Serialize format input k)   = Serialize format input (handler . (<$ state) . k)

-- | Run a 'Task' effect by performing the actions in 'IO'.
runTaskF :: TaskC m a -> m a
runTaskF = runTaskC

newtype TaskC m a = TaskC { runTaskC :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Member (Error SomeException) sig, Member (Lift IO) sig, Member (Reader TaskSession) sig, Member Resource sig, Member Telemetry sig, Member Timeout sig, Member Trace sig, Carrier sig m, MonadIO m) => Carrier (Task :+: sig) (TaskC m) where
  eff (R other) = TaskC . eff . handleCoercible $ other
  eff (L op) = case op of
    Parse parser blob k -> runParser blob parser >>= k
    Decorate algebra term k -> k (decoratorWithAlgebra algebra term)
    Semantic.Task.Diff terms k -> k (diffTermPair terms)
    Render renderer input k -> k (renderer input)
    Serialize format input k -> do
      formatStyle <- asks (Flag.choose IsTerminal Plain Colourful . configIsTerminal . config)
      k (runSerialize formatStyle format input)


-- | Log an 'Error.Error' at the specified 'Level'.
logError :: (Member Telemetry sig, Carrier sig m)
         => TaskSession
         -> Level
         -> Blob
         -> Error.Error String
         -> [(String, String)]
         -> m ()
logError TaskSession{..} level blob err =
  let shouldLogSource = configLogPrintSource config
      shouldColorize = Flag.switch IsTerminal Error.Colourize $ configIsTerminal config
  in writeLog level (Error.formatError shouldLogSource shouldColorize blob err)

data ParserCancelled = ParserTimedOut | AssignmentTimedOut
  deriving (Show, Typeable)

instance Exception ParserCancelled

-- | Parse a 'Blob' in 'IO'.
runParser :: (Member (Error SomeException) sig, Member (Lift IO) sig, Member (Reader TaskSession) sig, Member Resource sig, Member Telemetry sig, Member Timeout sig, Member Trace sig, Carrier sig m, MonadIO m)
          => Blob
          -> Parser term
          -> m term
runParser blob@Blob{..} parser = case parser of
  ASTParser language ->
    time "parse.tree_sitter_ast_parse" languageTag $ do
      config <- asks config
      parseToAST (configTreeSitterParseTimeout config) language blob
        >>= maybeM (throwError (SomeException ParserTimedOut))

  AssignmentParser    parser assignment -> runAssignment Assignment.assign    parser assignment
  DeterministicParser parser assignment -> runAssignment Deterministic.assign parser assignment

  MarkdownParser ->
    time "parse.cmark_parse" languageTag $
      let term = cmarkParser blobSource
      in length term `seq` pure term
  SomeParser parser -> SomeTerm <$> runParser blob parser
  where languageTag = pure . (,) ("language" :: String) . show $ blobLanguage blob
        errors :: (Syntax.Error :< fs, Apply Foldable fs, Apply Functor fs) => Term (Sum fs) Assignment.Loc -> [Error.Error String]
        errors = cata $ \ (In Assignment.Loc{..} syntax) -> case syntax of
          _ | Just err@Syntax.Error{} <- project syntax -> [Syntax.unError span err]
          _                                             -> fold syntax
        runAssignment :: ( Apply Foldable syntaxes
                         , Apply Functor syntaxes
                         , Element Syntax.Error syntaxes
                         , Member (Error SomeException) sig
                         , Member (Lift IO) sig
                         , Member (Reader TaskSession) sig
                         , Member Resource sig
                         , Member Telemetry sig
                         , Member Timeout sig
                         , Member Trace sig
                         , Carrier sig m
                         , MonadIO m
                         )
                      => (Source -> assignment (Term (Sum syntaxes) Assignment.Loc) -> ast -> Either (Error.Error String) (Term (Sum syntaxes) Assignment.Loc))
                      -> Parser ast
                      -> assignment (Term (Sum syntaxes) Assignment.Loc)
                      -> m (Term (Sum syntaxes) Assignment.Loc)
        runAssignment assign parser assignment = do
          taskSession <- ask
          let requestID' = ("github_request_id", requestID taskSession)
          let isPublic'  = ("github_is_public", show (isPublic taskSession))
          let logPrintFlag = configLogPrintSource . config $ taskSession
          let blobFields = ("path", if isPublic taskSession || Flag.toBool LogPrintSource logPrintFlag then blobPath blob else "<filtered>")
          let logFields = requestID' : isPublic' : blobFields : languageTag
          let shouldFailForTesting = configFailParsingForTesting $ config taskSession
          let shouldFailOnParsing = optionsFailOnParseError . configOptions $ config taskSession
          let shouldFailOnWarning = optionsFailOnWarning . configOptions $ config taskSession

          ast <- runParser blob parser `catchError` \ (SomeException err) -> do
            writeStat (increment "parse.parse_failures" languageTag)
            writeLog Error "failed parsing" (("task", "parse") : logFields)
            throwError (toException err)

          res <- timeout (configAssignmentTimeout (config taskSession)) . time "parse.assign" languageTag $
            case assign blobSource assignment ast of
              Left err -> do
                writeStat (increment "parse.assign_errors" languageTag)
                logError taskSession Error blob err (("task", "assign") : logFields)
                throwError (toException err)
              Right term -> do
                for_ (zip (errors term) [(0::Integer)..]) $ \ (err, i) -> case Error.errorActual err of
                  Just "ParseError" -> do
                    when (i == 0) $ writeStat (increment "parse.parse_errors" languageTag)
                    logError taskSession Warning blob err (("task", "parse") : logFields)
                    when (Flag.toBool FailOnParseError shouldFailOnParsing) (throwError (toException err))
                  _ -> do
                    when (i == 0) $ writeStat (increment "parse.assign_warnings" languageTag)
                    logError taskSession Warning blob err (("task", "assign") : logFields)
                    when (Flag.toBool FailOnWarning shouldFailOnWarning) (throwError (toException err))
                term <$ writeStat (count "parse.nodes" (length term) languageTag)
          case res of
            Just r | not (Flag.toBool FailTestParsing shouldFailForTesting) -> pure r
            _ -> do
              writeStat (increment "assign.assign_timeouts" languageTag)
              writeLog Error "assignment timeout" (("task", "assign") : logFields)
              throwError (SomeException AssignmentTimedOut)
