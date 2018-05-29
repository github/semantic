{-# LANGUAGE ConstraintKinds, GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeOperators #-}
module Semantic.Task
( Task
, TaskEff
, WrappedTask(..)
, Level(..)
, RAlgebra
-- * I/O
, IO.readBlob
, IO.readBlobs
, IO.readBlobPairs
, IO.readProject
, IO.findFiles
, IO.write
-- * Module Resolution
, resolutionMap
, Resolution
-- * Telemetry
, writeLog
, writeStat
, time
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
, defaultOptions
, configureOptionsForHandle
, terminalFormatter
, logfmtFormatter
-- * Interpreting
, runTask
, runTaskWithOptions
-- * Re-exports
, Distribute
, Eff
, Exc
, throwError
, SomeException
, Telemetry
) where

import           Analysis.Decorator (decoratorWithAlgebra)
import qualified Assigning.Assignment as Assignment
import qualified Control.Abstract as Analysis
import           Control.Monad
import           Control.Monad.Effect
import           Control.Monad.Effect.Exception
import           Control.Monad.Effect.Reader
import           Control.Monad.Effect.Trace
import           Data.Blob
import           Data.Bool
import           Data.ByteString.Builder
import           Data.Diff
import qualified Data.Error as Error
import           Data.Record
import           Data.Sum
import qualified Data.Syntax as Syntax
import           Data.Term
import           Diffing.Algorithm (Diffable)
import           Diffing.Interpreter
import           Parsing.CMark
import           Parsing.Parser
import           Parsing.TreeSitter
import           Prologue hiding (MonadError (..), project)
import           Semantic.Distribute
import qualified Semantic.IO as IO
import           Semantic.Resolution
import           Semantic.Log
import           Semantic.Queue
import           Semantic.Stat as Stat
import           Semantic.Telemetry
import           Serializing.Format hiding (Options)
import           System.Exit (die)
import           System.IO (stderr)

-- | A high-level task producing some result, e.g. parsing, diffing, rendering. 'Task's can also specify explicit concurrency via 'distribute', 'distributeFor', and 'distributeFoldMap'
type TaskEff = Eff '[Distribute WrappedTask
                    , Task
                    , Resolution
                    , IO.Files
                    , Reader Options
                    , Trace
                    , Telemetry
                    , Exc SomeException
                    , IO]

-- | A wrapper for a 'Task', to embed in other effects.
newtype WrappedTask a = WrapTask { unwrapTask :: TaskEff a }
  deriving (Applicative, Functor, Monad)

-- | A function to render terms or diffs.
type Renderer i o = i -> o

-- | A task which parses a 'Blob' with the given 'Parser'.
parse :: Member Task effs => Parser term -> Blob -> Eff effs term
parse parser = send . Parse parser

-- | A task running some 'Analysis.TermEvaluator' to completion.
analyze :: Member Task effs => (Analysis.TermEvaluator term location value effects a -> result) -> Analysis.TermEvaluator term location value effects a -> Eff effs result
analyze interpret analysis = send (Analyze interpret analysis)

-- | A task which decorates a 'Term' with values computed using the supplied 'RAlgebra' function.
decorate :: (Functor f, Member Task effs) => RAlgebra (TermF f (Record fields)) (Term f (Record fields)) field -> Term f (Record fields) -> Eff effs (Term f (Record (field ': fields)))
decorate algebra = send . Decorate algebra

-- | A task which diffs a pair of terms using the supplied 'Differ' function.
diff :: (Diffable syntax, Eq1 syntax, Hashable1 syntax, Traversable syntax, Member Task effs) => These (Term syntax (Record fields1)) (Term syntax (Record fields2)) -> Eff effs (Diff syntax (Record fields1) (Record fields2))
diff terms = send (Semantic.Task.Diff terms)

-- | A task which renders some input using the supplied 'Renderer' function.
render :: Member Task effs => Renderer input output -> input -> Eff effs output
render renderer = send . Render renderer

serialize :: Member Task effs => Format input -> input -> Eff effs Builder
serialize format = send . Serialize format

-- | Execute a 'Task' with the 'defaultOptions', yielding its result value in 'IO'.
--
-- > runTask = runTaskWithOptions defaultOptions
runTask :: TaskEff a -> IO a
runTask = runTaskWithOptions defaultOptions

-- | Execute a 'TaskEff' with the passed 'Options', yielding its result value in 'IO'.
runTaskWithOptions :: Options -> TaskEff a -> IO a
runTaskWithOptions options task = do
  options <- configureOptionsForHandle stderr options
  statter <- defaultStatsClient >>= newQueue sendStat
  logger <- newQueue logMessage options

  (result, stat) <- withTiming "run" [] $ do
    let run :: TaskEff a -> IO (Either SomeException a)
        run = runM . runError
                   . runTelemetry logger statter
                   . runTraceInTelemetry
                   . runReader options
                   . IO.runFiles
                   . runResolution
                   . runTaskF
                   . runDistribute (run . unwrapTask)
    run task
  queue statter stat

  closeQueue statter
  closeStatClient (asyncQueueExtra statter)
  closeQueue logger
  either (die . displayException) pure result

runTraceInTelemetry :: Member Telemetry effects => Eff (Trace ': effects) a -> Eff effects a
runTraceInTelemetry = interpret (\ (Trace str) -> writeLog Debug str [])


-- | An effect describing high-level tasks to be performed.
data Task output where
  Parse     :: Parser term -> Blob -> Task term
  Analyze  :: (Analysis.TermEvaluator term location value effects a -> result) -> Analysis.TermEvaluator term location value effects a -> Task result
  Decorate  :: Functor f => RAlgebra (TermF f (Record fields)) (Term f (Record fields)) field -> Term f (Record fields) -> Task (Term f (Record (field ': fields)))
  Diff      :: (Diffable syntax, Eq1 syntax, Hashable1 syntax, Traversable syntax) => These (Term syntax (Record fields1)) (Term syntax (Record fields2)) -> Task (Diff syntax (Record fields1) (Record fields2))
  Render    :: Renderer input output -> input -> Task output
  Serialize :: Format input -> input -> Task Builder

-- | Run a 'Task' effect by performing the actions in 'IO'.
runTaskF :: Members '[Reader Options, Telemetry, Exc SomeException, Trace, IO] effs => Eff (Task ': effs) a -> Eff effs a
runTaskF = interpret $ \ task -> case task of
  Parse parser blob -> runParser blob parser
  Analyze interpret analysis -> pure (interpret analysis)
  Decorate algebra term -> pure (decoratorWithAlgebra algebra term)
  Semantic.Task.Diff terms -> pure (diffTermPair terms)
  Render renderer input -> pure (renderer input)
  Serialize format input -> do
    formatStyle <- asks (bool Colourful Plain . optionsEnableColour)
    pure (runSerialize formatStyle format input)


-- | Log an 'Error.Error' at the specified 'Level'.
logError :: Member Telemetry effs => Options -> Level -> Blob -> Error.Error String -> [(String, String)] -> Eff effs ()
logError Options{..} level blob err = writeLog level (Error.formatError optionsPrintSource (optionsIsTerminal && optionsEnableColour) blob err)

data ParserCancelled = ParserTimedOut deriving (Show, Typeable)

instance Exception ParserCancelled

defaultTimeout :: Timeout
defaultTimeout = Milliseconds 5000

-- | Parse a 'Blob' in 'IO'.
runParser :: Members '[Reader Options, Telemetry, Exc SomeException, IO, Trace] effs => Blob -> Parser term -> Eff effs term
runParser blob@Blob{..} parser = case parser of
  ASTParser language ->
    time "parse.tree_sitter_ast_parse" languageTag $
      parseToAST defaultTimeout language blob
        >>= maybeM (throwError (SomeException ParserTimedOut))

  AssignmentParser parser assignment -> do
    ast <- runParser blob parser `catchError` \ (SomeException err) -> do
      writeStat (Stat.increment "parse.parse_failures" languageTag)
      writeLog Error "failed parsing" (("task", "parse") : blobFields)
      throwError (toException err)
    options <- ask
    time "parse.assign" languageTag $
      case Assignment.assign blobSource assignment ast of
        Left err -> do
          writeStat (Stat.increment "parse.assign_errors" languageTag)
          logError options Error blob err (("task", "assign") : blobFields)
          throwError (toException err)
        Right term -> do
          for_ (errors term) $ \ err -> case Error.errorActual err of
              Just "ParseError" -> do
                writeStat (Stat.increment "parse.parse_errors" languageTag)
                logError options Warning blob err (("task", "parse") : blobFields)
              _ -> do
                writeStat (Stat.increment "parse.assign_warnings" languageTag)
                logError options Warning blob err (("task", "assign") : blobFields)
                when (optionsFailOnWarning options) $ throwError (toException err)
          writeStat (Stat.count "parse.nodes" (length term) languageTag)
          pure term
  MarkdownParser ->
    time "parse.cmark_parse" languageTag $
      let term = cmarkParser blobSource
      in length term `seq` pure term
  SomeParser parser -> SomeTerm <$> runParser blob parser
  where blobFields = ("path", blobPath) : languageTag
        languageTag = maybe [] (pure . (,) ("language" :: String) . show) blobLanguage
        errors :: (Syntax.Error :< fs, Apply Foldable fs, Apply Functor fs) => Term (Sum fs) (Record Assignment.Location) -> [Error.Error String]
        errors = cata $ \ (In a syntax) -> case syntax of
          _ | Just err@Syntax.Error{} <- project syntax -> [Syntax.unError (getField a) err]
          _ -> fold syntax
