{-# LANGUAGE ApplicativeDo, FlexibleContexts #-}
module Semantic.CLI (main) where

import qualified Control.Carrier.Parse.Measured as Parse
import           Control.Effect.Reader
import           Control.Exception as Exc (displayException)
import           Data.Blob
import           Data.Blob.IO
import           Data.Handle
import qualified Data.Language as Language
import           Data.List (intercalate)
import           Data.Project
import qualified Data.Flag as Flag
import           Options.Applicative hiding (style)
import           Prologue
import           Semantic.Api hiding (File)
import           Semantic.Config
import qualified Semantic.Graph as Graph
import qualified Semantic.Task as Task
import           Semantic.Task.Files
import           Semantic.Telemetry
import qualified Semantic.Telemetry.Log as Log
import           Semantic.Version
import           Serializing.Format hiding (Options)
import           System.Exit (die)
import           System.FilePath
import qualified System.Path as Path
import qualified System.Path.PartClass as Path.PartClass

import Control.Concurrent (mkWeakThreadId, myThreadId)
import Control.Exception (Exception(..), throwTo)
import System.Posix.Signals
import System.Mem.Weak (deRefWeak)
import Proto.Semantic_JSON()

newtype SignalException = SignalException Signal
  deriving (Show)
instance Exception SignalException

installSignalHandlers :: IO ()
installSignalHandlers = do
  mainThreadId <- myThreadId
  weakTid <- mkWeakThreadId mainThreadId
  for_ [ sigABRT, sigBUS, sigHUP, sigILL, sigQUIT, sigSEGV,
          sigSYS, sigTERM, sigUSR1, sigUSR2, sigXCPU, sigXFSZ ] $ \sig ->
    installHandler sig (Catch $ sendException weakTid sig) Nothing
  where
    sendException weakTid sig = do
      m <- deRefWeak weakTid
      case m of
        Nothing  -> pure ()
        Just tid -> throwTo tid (toException $ SignalException sig)

main :: IO ()
main = do
  installSignalHandlers
  (options, task) <- customExecParser (prefs showHelpOnEmpty) arguments
  config <- defaultConfig options
  res <- withTelemetry config $ \ (TelemetryQueues logger statter _) ->
    Task.runTask (Task.TaskSession config "-" (optionsLogPathsOnError options) logger statter) (Parse.runParse task)
  either (die . displayException) pure res

-- | A parser for the application's command-line arguments.
--
--   Returns a 'Task' to read the input, run the requested operation, and write the output to the specified output path or stdout.
arguments :: ParserInfo (Options, Parse.ParseC Task.TaskC ())
arguments = info (version <*> helper <*> ((,) <$> optionsParser <*> argumentsParser)) description
  where
    version = infoOption versionString (long "version" <> short 'v' <> help "Output the version of the program")
    versionString = "semantic version " <> buildVersion <> " (" <> buildSHA <> ")"
    description = fullDesc <> header "semantic -- Semantic (syntax-aware) diffs, program analysis toolkit"

optionsParser :: Parser Options
optionsParser = do
  logLevel <- options [ ("error", Just Log.Error) , ("warning", Just Log.Warning) , ("info", Just Log.Info) , ("debug", Just Log.Debug) , ("none", Nothing)]
                      (long "log-level" <> value (Just Log.Warning) <> help "Log messages at or above this level, or disable logging entirely.")
  failOnWarning <- switch (long "fail-on-warning" <> help "Fail on assignment warnings.")
  failOnParseError <- switch (long "fail-on-parse-error" <> help "Fail on tree-sitter parse errors.")
  logPathsOnError <- switch (long "log-paths" <> help "Log source paths on parse and assignment error.")
  pure $ Options logLevel logPathsOnError (Flag.flag FailOnWarning failOnWarning) (Flag.flag FailOnParseError failOnParseError)

argumentsParser :: Parser (Parse.ParseC Task.TaskC ())
argumentsParser = do
  subparser <- hsubparser (diffCommand <> parseCommand <> graphCommand)
  output <- ToPath <$> pathOption (long "output" <> short 'o' <> help "Output path, defaults to stdout") <|> pure (ToHandle stdout)
  pure $ subparser >>= Task.write output

diffCommand :: Mod CommandFields (Parse.ParseC Task.TaskC Builder)
diffCommand = command "diff" (info diffArgumentsParser (progDesc "Compute changes between paths"))
  where
    diffArgumentsParser = do
      languageModes <- languageModes
      renderer <- flag  (parseDiffBuilder DiffSExpression) (parseDiffBuilder DiffSExpression) (long "sexpression" <> help "Output s-expression diff tree (default)")
              <|> flag'                                    (parseDiffBuilder DiffJSONTree)    (long "json"        <> help "Output JSON diff trees")
              <|> flag'                                    (parseDiffBuilder DiffJSONGraph)   (long "json-graph"  <> help "Output JSON diff trees")
              <|> flag'                                    (diffSummaryBuilder JSON)          (long "toc"         <> help "Output JSON table of contents diff summary")
              <|> flag'                                    (parseDiffBuilder DiffDotGraph)    (long "dot"         <> help "Output the diff as a DOT graph")
              <|> flag'                                    (parseDiffBuilder DiffShow)        (long "show"        <> help "Output using the Show instance (debug only, format subject to change without notice)")
      filesOrStdin <- Right <$> some ((,) <$> argument filePathReader (metavar "FILE_A") <*> argument filePathReader (metavar "FILE_B")) <|> pure (Left stdin)
      pure $ Task.readBlobPairs filesOrStdin >>= runReader languageModes . renderer

parseCommand :: Mod CommandFields (Parse.ParseC Task.TaskC Builder)
parseCommand = command "parse" (info parseArgumentsParser (progDesc "Generate parse trees for path(s)"))
  where
    parseArgumentsParser = do
      languageModes <- languageModes
      renderer
        <-  flag  (parseTermBuilder TermSExpression)
                  (parseTermBuilder TermSExpression)
                  (  long "sexpression"
                  <> help "Output s-expression parse trees (default)")
        <|> flag' (parseTermBuilder TermJSONTree)
                  (  long "json"
                  <> help "Output JSON parse trees")
        <|> flag' (parseTermBuilder TermJSONGraph)
                  (  long "json-graph"
                  <> help "Output JSON adjacency list")
        <|> flag' (parseSymbolsBuilder JSON)
                  (  long "symbols"
                  <> long "json-symbols"
                  <> help "Output JSON symbol list")
        <|> flag' (parseSymbolsBuilder Proto)
                  (  long "proto-symbols"
                  <> help "Output protobufs symbol list")
        <|> flag' (parseTermBuilder TermDotGraph)
                  (  long "dot"
                  <> help "Output DOT graph parse trees")
        <|> flag' (parseTermBuilder TermShow)
                  (  long "show"
                  <> help "Output using the Show instance (debug only, format subject to change without notice)")
        <|> flag' (parseTermBuilder TermQuiet)
                  (  long "quiet"
                  <> help "Don't produce output, but show timing stats")
      filesOrStdin <- FilesFromPaths <$> some (argument filePathReader (metavar "FILES..."))
                  <|> pure (FilesFromHandle stdin)
      pure $ Task.readBlobs filesOrStdin >>= runReader languageModes . renderer

graphCommand :: Mod CommandFields (Parse.ParseC Task.TaskC Builder)
graphCommand = command "graph" (info graphArgumentsParser (progDesc "Compute a graph for a directory or from a top-level entry point module"))
  where
    graphArgumentsParser = makeGraphTask
      <$> graphType
      <*> switch (long "packages" <> help "Include a vertex for the package, with edges from it to each module")
      <*> serializer
      <*> (readProjectRecursively <|> readProjectFromPaths)
    graphType =  flag  Graph.ImportGraph Graph.ImportGraph (long "imports" <> help "Compute an import graph (default)")
             <|> flag'                   Graph.CallGraph   (long "calls"   <> help "Compute a call graph")
    serializer =  flag (Task.serialize (DOT Graph.style)) (Task.serialize (DOT Graph.style)) (long "dot"  <> help "Output in DOT graph format (default)")
              <|> flag'                                   (Task.serialize JSON)              (long "json" <> help "Output JSON graph")
              <|> flag'                                   (Task.serialize Show)              (long "show" <> help "Output using the Show instance (debug only, format subject to change without notice)")
    readProjectFromPaths = makeReadProjectFromPathsTask
      <$> (   Just <$> some (strArgument (metavar "FILES..."))
          <|> flag' Nothing (long "stdin" <> help "Read a list of newline-separated paths to analyze from stdin."))
    makeReadProjectFromPathsTask maybePaths = do
      paths <- maybeM (liftIO (many getLine)) maybePaths
      blobs <- traverse readBlobFromFile' (fileForPath <$> paths)
      case paths of
        (x:_) -> pure $! Project (takeDirectory x) blobs (Language.languageForFilePath x) mempty
        _     -> pure $! Project "/" mempty Language.Unknown mempty
    readProjectRecursively = makeReadProjectRecursivelyTask
      <$> option auto (long "language" <> help "The language for the analysis.")
      <*> optional (pathOption (long "root" <> help "Root directory of project. Optional, defaults to entry file/directory." <> metavar "DIR"))
      <*> many (pathOption (long "exclude-dir" <> help "Exclude a directory (e.g. vendor)" <> metavar "DIR"))
      <*> argument path (metavar "PATH")
    makeReadProjectRecursivelyTask language rootDir excludeDirs dir = Task.readProject rootDir dir language excludeDirs
    makeGraphTask graphType includePackages serializer projectTask = projectTask >>= Graph.runGraph graphType includePackages >>= serializer

languageModes :: Parser Language.PerLanguageModes
languageModes = Language.PerLanguageModes
  <$> option auto (  long "python-mode"
                  <> help "The AST representation to use for Python sources"
                  <> metavar "ALaCarte|Precise"
                  <> value Language.ALaCarte
                  <> showDefault)

filePathReader :: ReadM File
filePathReader = fileForPath <$> str

path :: (Path.PartClass.FileDir fd) => ReadM (Path.AbsRel fd)
path = eitherReader Path.parse

pathOption :: Path.PartClass.FileDir fd => Mod OptionFields (Path.AbsRel fd) -> Parser (Path.AbsRel fd)
pathOption = option path

options :: Eq a => [(String, a)] -> Mod OptionFields a -> Parser a
options options fields = option (optionsReader options) (fields <> showDefaultWith (findOption options) <> metavar (intercalate "|" (fmap fst options)))
  where
    optionsReader options = eitherReader $ \ str -> maybe (Left ("expected one of: " <> intercalate ", " (fmap fst options))) (Right . snd) (find ((== str) . fst) options)
    findOption options value = maybe "" fst (find ((== value) . snd) options)
