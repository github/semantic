{-# LANGUAGE ApplicativeDo #-}
module Semantic.CLI (main) where

import           Control.Effect.Reader
import           Control.Exception as Exc (displayException)
import           Data.Blob
import           Data.Blob.IO
import           Data.Handle
import qualified Data.Language as Language
import           Data.List (intercalate)
import           Data.Project
import qualified Data.Text as T
import qualified Data.Flag as Flag
import           Options.Applicative hiding (style)
import           Prologue
import           Semantic.Api hiding (File)
import qualified Semantic.AST as AST
import           Semantic.Config
import qualified Semantic.Graph as Graph
import qualified Semantic.Task as Task
import qualified Semantic.Git as Git
import           Semantic.Task.Files
import           Semantic.Telemetry
import qualified Semantic.Telemetry.Log as Log
import           Semantic.Version
import           Serializing.Format hiding (Options)
import           System.Exit (die)
import           System.FilePath

import Control.Concurrent (mkWeakThreadId, myThreadId)
import Control.Exception (Exception(..), throwTo)
import Data.Typeable (Typeable)
import System.Posix.Signals
import System.Mem.Weak (deRefWeak)

newtype SignalException = SignalException Signal
  deriving (Show, Typeable)
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
    Task.runTask (Task.TaskSession config "-" (optionsLogPathsOnError options) logger statter) task
  either (die . displayException) pure res

-- | A parser for the application's command-line arguments.
--
--   Returns a 'Task' to read the input, run the requested operation, and write the output to the specified output path or stdout.
arguments :: ParserInfo (Options, Task.TaskEff ())
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

argumentsParser :: Parser (Task.TaskEff ())
argumentsParser = do
  subparser <- hsubparser (diffCommand <> parseCommand <>  tsParseCommand <> graphCommand)
  output <- ToPath <$> strOption (long "output" <> short 'o' <> help "Output path, defaults to stdout") <|> pure (ToHandle stdout)
  pure $ subparser >>= Task.write output

diffCommand :: Mod CommandFields (Task.TaskEff Builder)
diffCommand = command "diff" (info diffArgumentsParser (progDesc "Compute changes between paths"))
  where
    diffArgumentsParser = do
      renderer <- flag  (parseDiffBuilder DiffSExpression) (parseDiffBuilder DiffSExpression) (long "sexpression" <> help "Output s-expression diff tree (default)")
              <|> flag'                                    (parseDiffBuilder DiffJSONTree)    (long "json"        <> help "Output JSON diff trees")
              <|> flag'                                    (parseDiffBuilder DiffJSONGraph)   (long "json-graph"  <> help "Output JSON diff trees")
              <|> flag'                                    (diffSummaryBuilder JSON)          (long "toc"         <> help "Output JSON table of contents diff summary")
              <|> flag'                                    (parseDiffBuilder DiffDotGraph)    (long "dot"         <> help "Output the diff as a DOT graph")
              <|> flag'                                    (parseDiffBuilder DiffShow)        (long "show"        <> help "Output using the Show instance (debug only, format subject to change without notice)")
      filesOrStdin <- Right <$> some (Both <$> argument filePathReader (metavar "FILE_A") <*> argument filePathReader (metavar "FILE_B")) <|> pure (Left stdin)
      pure $ Task.readBlobPairs filesOrStdin >>= renderer

parseCommand :: Mod CommandFields (Task.TaskEff Builder)
parseCommand = command "parse" (info parseArgumentsParser (progDesc "Generate parse trees for path(s)"))
  where
    parseArgumentsParser = do
      languageModes <- Language.PerLanguageModes
        <$> option auto (  long "python-mode"
                        <> help "The AST representation to use for Python sources"
                        <> metavar "ALaCarte|Precise"
                        <> value Language.ALaCarte
                        <> showDefault)
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
      filesOrStdin <- FilesFromGitRepo
                      <$> option str (long "gitDir" <> help "A .git directory to read from")
                      <*> option shaReader (long "sha" <> help "The commit SHA1 to read from")
                      <*> ( ExcludePaths <$> many (option str (long "exclude" <> short 'x' <> help "Paths to exclude"))
                        <|> ExcludeFromHandle <$> flag' stdin (long "exclude-stdin" <> help "Exclude paths given to stdin")
                        <|> IncludePaths <$> many (option str (long "only" <> help "Only include the specified paths"))
                        <|> IncludePathsFromHandle <$> flag' stdin (long "only-stdin" <> help "Include only the paths given to stdin"))
                  <|> FilesFromPaths <$> some (argument filePathReader (metavar "FILES..."))
                  <|> pure (FilesFromHandle stdin)
      pure $ Task.readBlobs filesOrStdin >>= runReader languageModes . renderer

tsParseCommand :: Mod CommandFields (Task.TaskEff Builder)
tsParseCommand = command "ts-parse" (info tsParseArgumentsParser (progDesc "Generate raw tree-sitter parse trees for path(s)"))
  where
    tsParseArgumentsParser = do
      format <- flag  AST.SExpression AST.SExpression (long "sexpression" <> help "Output s-expression ASTs (default)")
            <|> flag'                 AST.JSON        (long "json"        <> help "Output JSON ASTs")
            <|> flag'                 AST.Quiet       (long "quiet"       <> help "Don't produce output, but show timing stats")
            <|> flag'                 AST.Show        (long "show"        <> help "Output using the Show instance (debug only, format subject to change without notice)")
      filesOrStdin <- FilesFromGitRepo
                      <$> option str (long "gitDir" <> help "A .git directory to read from")
                      <*> option shaReader (long "sha" <> help "The commit SHA1 to read from")
                      <*> ( ExcludePaths <$> many (option str (long "exclude" <> short 'x' <> help "Paths to exclude"))
                        <|> ExcludeFromHandle <$> flag' stdin (long "exclude-stdin" <> help "Exclude paths given to stdin")
                        <|> IncludePaths <$> many (option str (long "only" <> help "Only include the specified paths"))
                        <|> IncludePathsFromHandle <$> flag' stdin (long "only-stdin" <> help "Include only the paths given to stdin"))
                  <|> FilesFromPaths <$> some (argument filePathReader (metavar "FILES..."))
                  <|> pure (FilesFromHandle stdin)
      pure $ Task.readBlobs filesOrStdin >>= AST.runASTParse format

graphCommand :: Mod CommandFields (Task.TaskEff Builder)
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
      <*> optional (strOption (long "root" <> help "Root directory of project. Optional, defaults to entry file/directory." <> metavar "DIR"))
      <*> many (strOption (long "exclude-dir" <> help "Exclude a directory (e.g. vendor)" <> metavar "DIR"))
      <*> argument str (metavar "DIR")
    makeReadProjectRecursivelyTask language rootDir excludeDirs dir = Task.readProject rootDir dir language excludeDirs
    makeGraphTask graphType includePackages serializer projectTask = projectTask >>= Graph.runGraph graphType includePackages >>= serializer

shaReader :: ReadM Git.OID
shaReader = eitherReader parseSha
  where parseSha arg = if length arg == 40 || arg == "HEAD"
          then Right (Git.OID (T.pack arg))
          else Left (arg <> " is not a valid sha1")

filePathReader :: ReadM File
filePathReader = fileForPath <$> str

options :: Eq a => [(String, a)] -> Mod OptionFields a -> Parser a
options options fields = option (optionsReader options) (fields <> showDefaultWith (findOption options) <> metavar (intercalate "|" (fmap fst options)))
  where
    optionsReader options = eitherReader $ \ str -> maybe (Left ("expected one of: " <> intercalate ", " (fmap fst options))) (Right . snd) (find ((== str) . fst) options)
    findOption options value = maybe "" fst (find ((== value) . snd) options)
