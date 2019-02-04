{-# LANGUAGE ConstraintKinds, ExistentialQuantification, GADTs, KindSignatures, LambdaCase, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
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
, analyze
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
, Eff
, Error
, Lift
, throwError
, SomeException(..)
, Telemetry
) where

import           Analysis.Decorator (decoratorWithAlgebra)
import qualified Assigning.Assignment as Assignment
import qualified Assigning.Assignment.Deterministic as Deterministic
import qualified Control.Abstract as Analysis
import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Error
import           Control.Effect.Reader
import           Control.Effect.Resource
import           Control.Effect.Sum
import           Control.Effect.Trace
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Blob
import           Data.Bool
import           Data.ByteString.Builder
import           Data.Coerce
import           Data.Diff
import qualified Data.Error as Error
import           Data.Location
import           Data.Source (Source)
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
import qualified Semantic.Task.Files as Files
import           Semantic.Timeout
import           Semantic.Resolution
import           Semantic.Telemetry
import           Serializing.Format hiding (Options)

-- | A high-level task producing some result, e.g. parsing, diffing, rendering. 'Task's can also specify explicit concurrency via 'distribute', 'distributeFor', and 'distributeFoldMap'
type TaskEff
  = Eff (TaskC
  ( Eff (ResolutionC
  ( Eff (Files.FilesC
  ( Eff (ReaderC Config
  ( Eff (TraceInTelemetryC
  ( Eff (TelemetryC
  ( Eff (ErrorC SomeException
  ( Eff (TimeoutC
  ( Eff (ResourceC
  ( Eff (DistributeC
  ( Eff (LiftC IO)))))))))))))))))))))

-- | A function to render terms or diffs.
type Renderer i o = i -> o

-- | A task which parses a 'Blob' with the given 'Parser'.
parse :: (Member Task sig, Carrier sig m)
      => Parser term
      -> Blob
      -> m term
parse parser blob = send (Parse parser blob ret)

-- | A task running some 'Analysis.Evaluator' to completion.
analyze :: (Member Task sig, Carrier sig m)
        => (Analysis.Evaluator term address value m a -> result)
        -> Analysis.Evaluator term address value m a
        -> m result
analyze interpret analysis = send (Analyze interpret analysis ret)

-- | A task which decorates a 'Term' with values computed using the supplied 'RAlgebra' function.
decorate :: (Functor f, Member Task sig, Carrier sig m)
         => RAlgebra (TermF f Location) (Term f Location) field
         -> Term f Location
         -> m (Term f field)
decorate algebra term = send (Decorate algebra term ret)

-- | A task which diffs a pair of terms using the supplied 'Differ' function.
diff :: (Diffable syntax, Eq1 syntax, Hashable1 syntax, Traversable syntax, Member Task sig, Carrier sig m)
     => These (Term syntax ann) (Term syntax ann)
     -> m (Diff syntax ann ann)
diff terms = send (Semantic.Task.Diff terms ret)

-- | A task which renders some input using the supplied 'Renderer' function.
render :: (Member Task sig, Carrier sig m)
       => Renderer input output
       -> input
       -> m output
render renderer input = send (Render renderer input ret)

serialize :: (Member Task sig, Carrier sig m)
          => Format input
          -> input
          -> m Builder
serialize format input = send (Serialize format input ret)

data TaskSession
  = TaskSession
  { config    :: Config
  , requestID :: String
  , logger    :: LogQueue
  , statter   :: StatQueue
  }

-- | Execute a 'TaskEff' yielding its result value in 'IO'.
runTask :: TaskSession -> TaskEff a -> IO (Either SomeException a)
runTask TaskSession{..} task = do
  (result, stat) <- withTiming "run" [] $ do
    let run :: TaskEff a -> IO (Either SomeException a)
        run
          = runM
          . runDistribute
          . runResource (runM . runDistribute)
          . runTimeout (runM . runDistribute . runResource (runM . runDistribute))
          . runError
          . runTelemetry logger statter
          . runTraceInTelemetry
          . runReader config
          . Files.runFiles
          . runResolution
          . runTaskF
    run task
  queueStat statter stat
  pure result

-- | Execute a 'TaskEff' yielding its result value in 'IO' using all default options and configuration.
runTaskWithOptions :: Options -> TaskEff a -> IO (Either SomeException a)
runTaskWithOptions options task = withOptions options $ \ config logger statter ->
  runTask (TaskSession config "-" logger statter) task

-- | Yield config and telemetry queues for options.
withOptions :: Options -> (Config -> LogQueue -> StatQueue -> IO a) -> IO a
withOptions options with = do
  config <- defaultConfig options
  withTelemetry config (\ (TelemetryQueues logger statter _) -> with config logger statter)

runTraceInTelemetry :: (Member Telemetry sig, Carrier sig m, Monad m)
                    => Eff (TraceInTelemetryC m) a
                    -> m a
runTraceInTelemetry = runTraceInTelemetryC . interpret

newtype TraceInTelemetryC m a = TraceInTelemetryC { runTraceInTelemetryC :: m a }

instance (Member Telemetry sig, Carrier sig m, Monad m) => Carrier (Trace :+: sig) (TraceInTelemetryC m) where
  ret = TraceInTelemetryC . ret
  eff = TraceInTelemetryC . handleSum
    (eff . handleCoercible)
    (\ (Trace str k) -> writeLog Debug str [] >> runTraceInTelemetryC k)


-- | An effect describing high-level tasks to be performed.
data Task (m :: * -> *) k
  = forall term . Parse (Parser term) Blob (term -> k)
  | forall term address value m a result . Analyze (Analysis.Evaluator term address value m a -> result) (Analysis.Evaluator term address value m a) (result -> k)
  | forall f field . Functor f => Decorate (RAlgebra (TermF f Location) (Term f Location) field) (Term f Location) (Term f field -> k)
  | forall syntax ann . (Diffable syntax, Eq1 syntax, Hashable1 syntax, Traversable syntax) => Diff (These (Term syntax ann) (Term syntax ann)) (Diff syntax ann ann -> k)
  | forall input output . Render (Renderer input output) input (output -> k)
  | forall input . Serialize (Format input) input (Builder -> k)

deriving instance Functor (Task m)

instance HFunctor Task where
  hmap _ = coerce

instance Effect Task where
  handle state handler (Parse parser blob k) = Parse parser blob (handler . (<$ state) . k)
  handle state handler (Analyze run analysis k) = Analyze run analysis (handler . (<$ state) . k)
  handle state handler (Decorate decorator term k) = Decorate decorator term (handler . (<$ state) . k)
  handle state handler (Semantic.Task.Diff terms k) = Semantic.Task.Diff terms (handler . (<$ state) . k)
  handle state handler (Render renderer input k) = Render renderer input (handler . (<$ state) . k)
  handle state handler (Serialize format input k) = Serialize format input (handler . (<$ state) . k)

-- | Run a 'Task' effect by performing the actions in 'IO'.
runTaskF :: ( Member (Error SomeException) sig
            , Member (Lift IO) sig
            , Member (Reader Config) sig
            , Member Resource sig
            , Member Telemetry sig
            , Member Timeout sig
            , Member Trace sig
            , Carrier sig m
            , MonadIO m
            )
         => Eff (TaskC m) a
         -> m a
runTaskF = runTaskC . interpret

newtype TaskC m a = TaskC { runTaskC :: m a }

instance (Member (Error SomeException) sig, Member (Lift IO) sig, Member (Reader Config) sig, Member Resource sig, Member Telemetry sig, Member Timeout sig, Member Trace sig, Carrier sig m, MonadIO m) => Carrier (Task :+: sig) (TaskC m) where
  ret = TaskC . ret
  eff = TaskC . handleSum (eff . handleCoercible) (\case
    Parse parser blob k -> runParser blob parser >>= runTaskC . k
    Analyze interpret analysis k -> runTaskC (k (interpret analysis))
    Decorate algebra term k -> runTaskC (k (decoratorWithAlgebra algebra term))
    Semantic.Task.Diff terms k -> runTaskC (k (diffTermPair terms))
    Render renderer input k -> runTaskC (k (renderer input))
    Serialize format input k -> do
      formatStyle <- asks (bool Plain Colourful . configIsTerminal)
      runTaskC (k (runSerialize formatStyle format input)))


-- | Log an 'Error.Error' at the specified 'Level'.
logError :: (Member Telemetry sig, Carrier sig m)
         => Config
         -> Level
         -> Blob
         -> Error.Error String
         -> [(String, String)]
         -> m ()
logError Config{..} level blob err = writeLog level (Error.formatError configLogPrintSource configIsTerminal blob err)

data ParserCancelled = ParserTimedOut | AssignmentTimedOut
  deriving (Show, Typeable)

instance Exception ParserCancelled

-- | Parse a 'Blob' in 'IO'.
runParser :: (Member (Error SomeException) sig, Member (Lift IO) sig, Member (Reader Config) sig, Member Resource sig, Member Telemetry sig, Member Timeout sig, Member Trace sig, Carrier sig m, MonadIO m)
          => Blob
          -> Parser term
          -> m term
runParser blob@Blob{..} parser = case parser of
  ASTParser language ->
    time "parse.tree_sitter_ast_parse" languageTag $ do
      config <- ask
      parseToAST (configTreeSitterParseTimeout config) language blob
        >>= maybeM (throwError (SomeException ParserTimedOut))

  AssignmentParser    parser assignment -> runAssignment Assignment.assign    parser assignment
  DeterministicParser parser assignment -> runAssignment Deterministic.assign parser assignment

  MarkdownParser ->
    time "parse.cmark_parse" languageTag $
      let term = cmarkParser blobSource
      in length term `seq` pure term
  SomeParser parser -> SomeTerm <$> runParser blob parser
  where languageTag = pure . (,) ("language" :: String) . show $ blobLanguage
        errors :: (Syntax.Error :< fs, Apply Foldable fs, Apply Functor fs) => Term (Sum fs) Assignment.Location -> [Error.Error String]
        errors = cata $ \ (In Assignment.Location{..} syntax) -> case syntax of
          _ | Just err@Syntax.Error{} <- project syntax -> [Syntax.unError locationSpan err]
          _ -> fold syntax
        runAssignment :: ( Apply Foldable syntaxes
                         , Apply Functor syntaxes
                         , Element Syntax.Error syntaxes
                         , Member (Error SomeException) sig
                         , Member (Lift IO) sig
                         , Member (Reader Config) sig
                         , Member Resource sig
                         , Member Telemetry sig
                         , Member Timeout sig
                         , Member Trace sig
                         , Carrier sig m
                         , MonadIO m
                         )
                      => (Source -> assignment (Term (Sum syntaxes) Assignment.Location) -> ast -> Either (Error.Error String) (Term (Sum syntaxes) Assignment.Location))
                      -> Parser ast
                      -> assignment (Term (Sum syntaxes) Assignment.Location)
                      -> m (Term (Sum syntaxes) Assignment.Location)
        runAssignment assign parser assignment = do
          config <- ask
          let blobFields = ("path", if configLogPrintSource config then blobPath else "<filtered>") : languageTag
          ast <- runParser blob parser `catchError` \ (SomeException err) -> do
            writeStat (increment "parse.parse_failures" languageTag)
            writeLog Error "failed parsing" (("task", "parse") : blobFields)
            throwError (toException err)

          res <- timeout (configAssignmentTimeout config) . time "parse.assign" languageTag $
            case assign blobSource assignment ast of
              Left err -> do
                writeStat (increment "parse.assign_errors" languageTag)
                logError config Error blob err (("task", "assign") : blobFields)
                throwError (toException err)
              Right term -> do
                for_ (zip (errors term) [(0::Integer)..]) $ \ (err, i) -> case Error.errorActual err of
                  Just "ParseError" -> do
                    when (i == 0) $ writeStat (increment "parse.parse_errors" languageTag)
                    logError config Warning blob err (("task", "parse") : blobFields)
                    when (optionsFailOnParseError (configOptions config)) $ throwError (toException err)
                  _ -> do
                    when (i == 0) $ writeStat (increment "parse.assign_warnings" languageTag)
                    logError config Warning blob err (("task", "assign") : blobFields)
                    when (optionsFailOnWarning (configOptions config)) $ throwError (toException err)
                term <$ writeStat (count "parse.nodes" (length term) languageTag)
          case res of
            Just r -> pure r
            Nothing -> do
              writeStat (increment "assign.assign_timeouts" languageTag)
              writeLog Error "assignment timeout" (("task", "assign") : blobFields)
              throwError (SomeException AssignmentTimedOut)
