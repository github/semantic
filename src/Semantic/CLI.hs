{-# LANGUAGE ApplicativeDo, RankNTypes #-}
module Semantic.CLI
( main
-- Testing
, Diff.runDiff
, Parse.runParse
) where

import           Control.Monad.IO.Class
import           Data.Language (ensureLanguage)
import           Data.List (intercalate, uncons)
import           Data.List.Split (splitWhen)
import           Data.Project
import           Options.Applicative hiding (style)
import           Prologue
import           Rendering.Renderer
import qualified Semantic.AST as AST
import           Semantic.Config
import qualified Semantic.Diff as Diff
import qualified Semantic.Graph as Graph
import           Semantic.IO as IO
import qualified Semantic.Parse as Parse
import qualified Semantic.Task as Task
import qualified Semantic.Telemetry.Log as Log
import           Semantic.Version
import           System.FilePath
import           Serializing.Format hiding (Options)
import           Text.Read

main :: IO ()
main = customExecParser (prefs showHelpOnEmpty) arguments >>= uncurry Task.runTaskWithOptions

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
  requestId <- optional (strOption $ long "request-id" <> help "A string to use as the request identifier for any logged messages." <> metavar "id")
  failOnWarning <- switch (long "fail-on-warning" <> help "Fail on assignment warnings.")
  failOnParseError <- switch (long "fail-on-parse-error" <> help "Fail on tree-sitter parse errors.")
  pure $ Options logLevel requestId failOnWarning failOnParseError

argumentsParser :: Parser (Task.TaskEff ())
argumentsParser = do
  subparser <- hsubparser (diffCommand <> parseCommand <>  tsParseCommand <> graphCommand)
  output <- ToPath <$> strOption (long "output" <> short 'o' <> help "Output path, defaults to stdout") <|> pure (ToHandle stdout)
  pure $ subparser >>= Task.write output

diffCommand :: Mod CommandFields (Task.TaskEff Builder)
diffCommand = command "diff" (info diffArgumentsParser (progDesc "Compute changes between paths"))
  where
    diffArgumentsParser = do
      renderer <- flag  (Diff.runDiff SExpressionDiffRenderer) (Diff.runDiff SExpressionDiffRenderer) (long "sexpression" <> help "Output s-expression diff tree (default)")
              <|> flag'                                        (Diff.runDiff JSONDiffRenderer)        (long "json"        <> help "Output JSON diff trees")
              <|> flag'                                        (Diff.runDiff JSONGraphDiffRenderer)   (long "json-graph"  <> help "Output JSON diff trees")
              <|> flag'                                        (Diff.runDiff ToCDiffRenderer)         (long "toc"         <> help "Output JSON table of contents diff summary")
              <|> flag'                                        (Diff.runDiff DOTDiffRenderer)         (long "dot"         <> help "Output the diff as a DOT graph")
              <|> flag'                                        (Diff.runDiff ShowDiffRenderer)        (long "show"        <> help "Output using the Show instance (debug only, format subject to change without notice)")
      filesOrStdin <- Right <$> some (both <$> argument filePathReader (metavar "FILE_A") <*> argument filePathReader (metavar "FILE_B")) <|> pure (Left stdin)
      pure $ Task.readBlobPairs filesOrStdin >>= renderer

parseCommand :: Mod CommandFields (Task.TaskEff Builder)
parseCommand = command "parse" (info parseArgumentsParser (progDesc "Generate parse trees for path(s)"))
  where
    parseArgumentsParser = do
      renderer <- flag  (Parse.runParse SExpressionTermRenderer) (Parse.runParse SExpressionTermRenderer) (long "sexpression" <> help "Output s-expression parse trees (default)")
              <|> flag'                                          (Parse.runParse JSONTermRenderer)        (long "json"        <> help "Output JSON parse trees")
              <|> flag'                                          (Parse.runParse JSONGraphTermRenderer)   (long "json-graph"  <> help "Output JSON adjacency list")
              <|> flag'                                          (Parse.runParse . SymbolsTermRenderer)   (long "symbols"     <> help "Output JSON symbol list")
                   <*> (option symbolFieldsReader (  long "fields"
                                                 <> help "Comma delimited list of specific fields to return (symbols output only)."
                                                 <> metavar "FIELDS")
                  <|> pure defaultSymbolFields)
              <|> flag'                                          (Parse.runParse DOTTermRenderer)         (long "dot"          <> help "Output DOT graph parse trees")
              <|> flag'                                          (Parse.runParse ShowTermRenderer)        (long "show"         <> help "Output using the Show instance (debug only, format subject to change without notice)")
              <|> flag'                                          (Parse.runParse QuietTermRenderer)        (long "quiet"        <> help "Don't produce output, but show timing stats")
      filesOrStdin <- Right <$> some (argument filePathReader (metavar "FILES...")) <|> pure (Left stdin)
      pure $ Task.readBlobs filesOrStdin >>= renderer

    -- Example: semantic parse --symbols --fields=symbol,path,language,kind,line,span
    symbolFieldsReader = eitherReader (Right . parseSymbolFields)

tsParseCommand :: Mod CommandFields (Task.TaskEff Builder)
tsParseCommand = command "ts-parse" (info tsParseArgumentsParser (progDesc "Print specialized tree-sitter ASTs for path(s)"))
  where
    tsParseArgumentsParser = do
      format <- flag  AST.SExpression AST.SExpression (long "sexpression" <> help "Output s-expression ASTs (default)")
            <|> flag'                 AST.JSON        (long "json"        <> help "Output JSON ASTs")
            <|> flag'                 AST.Quiet       (long "quiet"       <> help "Do not print anything to the console")
            <|> flag'                 AST.Show        (long "show"        <> help "Output using the Show instance (debug only, format subject to change without notice)")
      filesOrStdin <- Right <$> some (argument filePathReader (metavar "FILES...")) <|> pure (Left stdin)
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
      <$> option auto (long "language" <> help "The language for the analysis.")
      <*> (   Just <$> some (strArgument (metavar "FILES..."))
          <|> flag' Nothing (long "stdin" <> help "Read a list of newline-separated paths to analyze from stdin."))
    makeReadProjectFromPathsTask language maybePaths = do
      paths <- maybeM (liftIO (many getLine)) maybePaths
      blobs <- traverse IO.readBlob (flip File language <$> paths)
      pure $! Project (takeDirectory (maybe "/" fst (uncons paths))) blobs language []
    readProjectRecursively = makeReadProjectRecursivelyTask
      <$> optional (strOption (long "root" <> help "Root directory of project. Optional, defaults to entry file/directory." <> metavar "DIR"))
      <*> many (strOption (long "exclude-dir" <> help "Exclude a directory (e.g. vendor)" <> metavar "DIR"))
      <*> argument filePathReader (metavar "DIR:LANGUAGE | FILE")
    makeReadProjectRecursivelyTask rootDir excludeDirs File{..} = Task.readProject rootDir filePath fileLanguage excludeDirs
    makeGraphTask graphType includePackages serializer projectTask = projectTask >>= Graph.runGraph graphType includePackages >>= serializer

filePathReader :: ReadM File
filePathReader = eitherReader parseFilePath
  where
    parseFilePath arg = case splitWhen (== ':') arg of
        [a, b] | Just lang <- readMaybe b >>= ensureLanguage -> Right (File a lang)
               | Just lang <- readMaybe a >>= ensureLanguage -> Right (File b lang)
        [path] -> maybe (Left $ "Cannot identify language for path: " <> path) (Right . File path) (ensureLanguage (languageForFilePath path))
        args -> Left ("cannot parse `" <> join args <> "`\nexpecting FILE:LANGUAGE or just FILE")

options :: Eq a => [(String, a)] -> Mod OptionFields a -> Parser a
options options fields = option (optionsReader options) (fields <> showDefaultWith (findOption options) <> metavar (intercalate "|" (fmap fst options)))
  where
    optionsReader options = eitherReader $ \ str -> maybe (Left ("expected one of: " <> intercalate ", " (fmap fst options))) (Right . snd) (find ((== str) . fst) options)
    findOption options value = maybe "" fst (find ((== value) . snd) options)
