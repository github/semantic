{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TypeOperators, UndecidableInstances #-}
module Semantic.Task
( Task
, TaskEff
, WrappedTask(..)
, Level(..)
, RAlgebra
, Differ
-- * I/O
, IO.readBlobs
, IO.readBlobPairs
, IO.writeToOutput
-- * Telemetry
, writeLog
, writeStat
, time
-- * High-level flow
, parse
, parseModule
, parseModules
, parsePackage
, analyze
, decorate
, diff
, render
, importGraph
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

import qualified Analysis.Abstract.ImportGraph as Abstract
import           Analysis.Abstract.Evaluating
import           Analysis.Decorator (decoratorWithAlgebra)
import qualified Assigning.Assignment as Assignment
import qualified Control.Abstract.Analysis as Analysis
import           Control.Monad.Effect.Exception
import           Control.Monad.Effect.Internal as Eff hiding (run)
import           Control.Monad.Effect.Reader
import           Control.Monad.Effect.Run as Run
import           Data.Abstract.Address
import qualified Data.Abstract.Evaluatable as Analysis
import           Data.Abstract.FreeVariables
import           Data.Abstract.Located
import           Data.Abstract.Module
import           Data.Abstract.Package as Package
import           Data.Abstract.Value (Value)
import           Data.Blob
import qualified Data.ByteString as B
import           Data.Diff
import qualified Data.Error as Error
import           Data.Record
import qualified Data.Syntax as Syntax
import           Data.Term
import           Parsing.CMark
import           Parsing.Parser
import           Parsing.TreeSitter
import           Prologue hiding (MonadError(..))
import           Semantic.Distribute
import qualified Semantic.IO as IO
import           Semantic.Log
import           Semantic.Queue
import           Semantic.Stat as Stat
import           Semantic.Telemetry
import           System.Exit (die)
import           System.IO (stderr)

data Task output where
  Parse         :: Parser term -> Blob -> Task term
  Analyze       :: Analysis.SomeAnalysis m result -> Task result
  Decorate      :: Functor f => RAlgebra (TermF f (Record fields)) (Term f (Record fields)) field -> Term f (Record fields) -> Task (Term f (Record (field ': fields)))
  Diff          :: Differ syntax ann1 ann2 -> Term syntax ann1 -> Term syntax ann2 -> Task (Diff syntax ann1 ann2)
  Render        :: Renderer input output -> input -> Task output

-- | A high-level task producing some result, e.g. parsing, diffing, rendering. 'Task's can also specify explicit concurrency via 'distribute', 'distributeFor', and 'distributeFoldMap'
type TaskEff = Eff '[Distribute WrappedTask, Task, IO.Files, Reader Options, Telemetry, Exc SomeException, IO]

-- | A wrapper for a 'Task', to embed in other effects.
newtype WrappedTask a = WrapTask { unwrapTask :: TaskEff a }
  deriving (Applicative, Functor, Monad)

-- | A function to compute the 'Diff' for a pair of 'Term's with arbitrary syntax functor & annotation types.
type Differ syntax ann1 ann2 = Term syntax ann1 -> Term syntax ann2 -> Diff syntax ann1 ann2

-- | A function to render terms or diffs.
type Renderer i o = i -> o

-- | A task which parses a 'Blob' with the given 'Parser'.
parse :: Member Task effs => Parser term -> Blob -> Eff effs term
parse parser = send . Parse parser

-- | Parse a file into a 'Module'.
parseModule :: Members '[IO.Files, Task] effs => Parser term -> Maybe FilePath -> FilePath -> Eff effs (Module term)
parseModule parser rootDir path = do
  blob <- head <$> IO.readBlobs (Right [(path, IO.languageForFilePath path)])
  moduleForBlob rootDir blob <$> parse parser blob

-- | Parse a list of files into 'Module's.
parseModules :: Members '[IO.Files, Task] effs => Parser term -> FilePath -> [FilePath] -> Eff effs [Module term]
parseModules parser rootDir = traverse (parseModule parser (Just rootDir))

-- | Parse a list of files into a 'Package'.
parsePackage :: Members '[IO.Files, Task] effs => PackageName -> Parser term -> FilePath -> [FilePath] -> Eff effs (Package term)
parsePackage name parser rootDir paths = Package (PackageInfo name Nothing) . Package.fromModules <$> parseModules parser rootDir paths


-- | A task running some 'Analysis.MonadAnalysis' to completion.
analyze :: Member Task effs => Analysis.SomeAnalysis m result -> Eff effs result
analyze = send . Analyze

-- | A task which decorates a 'Term' with values computed using the supplied 'RAlgebra' function.
decorate :: (Functor f, Member Task effs) => RAlgebra (TermF f (Record fields)) (Term f (Record fields)) field -> Term f (Record fields) -> Eff effs (Term f (Record (field ': fields)))
decorate algebra = send . Decorate algebra

-- | A task which diffs a pair of terms using the supplied 'Differ' function.
diff :: Member Task effs => Differ syntax ann1 ann2 -> Term syntax ann1 -> Term syntax ann2 -> Eff effs (Diff syntax ann1 ann2)
diff differ term1 term2 = send (Semantic.Task.Diff differ term1 term2)

-- | A task which renders some input using the supplied 'Renderer' function.
render :: Member Task effs => Renderer input output -> input -> Eff effs output
render renderer = send . Render renderer


importGraph :: (Apply Eq1 syntax, Apply Analysis.Evaluatable syntax, Apply FreeVariables1 syntax, Apply Functor syntax, Apply Ord1 syntax, Apply Show1 syntax, Member Syntax.Identifier syntax, Member Task effs, Ord ann, Show ann) => Package (Term (Union syntax) ann) -> Eff effs B.ByteString
importGraph package = renderGraph <$> analyze (Analysis.SomeAnalysis (Analysis.evaluatePackage package `asAnalysisForTypeOfPackage` package))
  where asAnalysisForTypeOfPackage :: Abstract.ImportGraphing (Evaluating (Located Precise term) term (Value (Located Precise term))) effects value -> Package term -> Abstract.ImportGraphing (Evaluating (Located Precise term) term (Value (Located Precise term))) effects value
        asAnalysisForTypeOfPackage = const

        renderGraph result = case result of
          (Right (Right (Right (Right (Right (_, graph))))), _) -> Abstract.renderImportGraph graph
          _ -> error "blah"


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
        run task = Run.run task (Action (run . unwrapTask)) options (Queues logger statter)
    run task
  queue statter stat

  closeQueue statter
  closeStatClient (asyncQueueExtra statter)
  closeQueue logger
  either (die . displayException) pure result

logError :: Member Telemetry effs => Options -> Level -> Blob -> Error.Error String -> [(String, String)] -> Eff effs ()
logError Options{..} level blob err = writeLog level (Error.formatError optionsPrintSource (optionsIsTerminal && optionsEnableColour) blob err)

runParser :: Members '[Reader Options, Telemetry, Exc SomeException, IO] effs => Blob -> Parser term -> Eff effs term
runParser blob@Blob{..} parser = case parser of
  ASTParser language ->
    time "parse.tree_sitter_ast_parse" languageTag $
      IO.rethrowing (parseToAST language blob)
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
          writeStat (Stat.count "parse.nodes" (length term) languageTag)
          pure term
  MarkdownParser ->
    time "parse.cmark_parse" languageTag $
      let term = cmarkParser blobSource
      in length term `seq` pure term
  where blobFields = ("path", blobPath) : languageTag
        languageTag = maybe [] (pure . (,) ("language" :: String) . show) blobLanguage
        errors :: (Syntax.Error :< fs, Apply Foldable fs, Apply Functor fs) => Term (Union fs) (Record Assignment.Location) -> [Error.Error String]
        errors = cata $ \ (In a syntax) -> case syntax of
          _ | Just err@Syntax.Error{} <- prj syntax -> [Syntax.unError (getField a) err]
          _ -> fold syntax


runTaskF :: Members '[Reader Options, Telemetry, Exc SomeException, IO] effs => Eff (Task ': effs) a -> Eff effs a
runTaskF = interpret $ \ task -> case task of
  Parse parser blob -> runParser blob parser
  Analyze analysis -> pure (Analysis.runSomeAnalysis analysis)
  Decorate algebra term -> pure (decoratorWithAlgebra algebra term)
  Semantic.Task.Diff differ term1 term2 -> pure (differ term1 term2)
  Render renderer input -> pure (renderer input)

instance (Members '[Reader Options, Telemetry, Exc SomeException, IO] effects, Run effects result rest) => Run (Task ': effects) result rest where
  run = run . runTaskF
