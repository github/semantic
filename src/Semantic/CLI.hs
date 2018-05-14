{-# LANGUAGE ApplicativeDo, TemplateHaskell #-}
module Semantic.CLI
( main
-- Testing
, runDiff
, runParse
) where

import           Data.ByteString.Builder
import           Data.File
import           Data.Language (Language)
import           Data.List (intercalate)
import           Data.List.Split (splitWhen)
import           Data.Output
import           Data.Version (showVersion)
import           Development.GitRev
import           Options.Applicative hiding (style)
import qualified Paths_semantic as Library (version)
import           Prologue
import           Rendering.Renderer
import qualified Semantic.Diff as Semantic (diffBlobPairs)
import           Semantic.Graph as Semantic (Graph, GraphType(..), Vertex, graph, style)
import           Semantic.IO as IO
import qualified Semantic.Log as Log
import qualified Semantic.Parse as Semantic (parseBlobs, astParseBlobs)
import qualified Semantic.Task as Task
import           Serializing.Format
import           Text.Read

main :: IO ()
main = customExecParser (prefs showHelpOnEmpty) arguments >>= uncurry Task.runTaskWithOptions

runDiff :: SomeRenderer DiffRenderer -> Either (Handle 'IO.ReadMode) [Both File] -> Task.TaskEff Builder
runDiff (SomeRenderer diffRenderer) = fmap toOutput . Semantic.diffBlobPairs diffRenderer <=< Task.readBlobPairs

runParse :: SomeRenderer TermRenderer -> Either (Handle 'IO.ReadMode) [File] -> Task.TaskEff Builder
runParse (SomeRenderer parseTreeRenderer) = fmap toOutput . Semantic.parseBlobs parseTreeRenderer <=< Task.readBlobs

runASTParse :: SomeRenderer TermRenderer -> Either (Handle 'IO.ReadMode) [File] -> Task.TaskEff Builder
runASTParse (SomeRenderer parseTreeRenderer) = fmap toOutput . Semantic.astParseBlobs parseTreeRenderer <=< Task.readBlobs

runGraph :: Semantic.GraphType -> Maybe FilePath -> FilePath -> Language -> [FilePath] -> Task.TaskEff (Graph Vertex)
runGraph graphType rootDir dir excludeDirs = Semantic.graph graphType <=< Task.readProject rootDir dir excludeDirs

-- | A parser for the application's command-line arguments.
--
--   Returns a 'Task' to read the input, run the requested operation, and write the output to the specified output path or stdout.
arguments :: ParserInfo (Log.Options, Task.TaskEff ())
arguments = info (version <*> helper <*> ((,) <$> optionsParser <*> argumentsParser)) description
  where
    version = infoOption versionString (long "version" <> short 'v' <> help "Output the version of the program")
    versionString = "semantic version " <> showVersion Library.version <> " (" <> $(gitHash) <> ")"
    description = fullDesc <> header "semantic -- Parse and diff semantically"

    optionsParser = do
      disableColour <- not <$> switch (long "disable-colour" <> long "disable-color" <> help "Disable ANSI colors in log messages even if the terminal is a TTY.")
      logLevel <- options [ ("error", Just Log.Error) , ("warning", Just Log.Warning) , ("info", Just Log.Info) , ("debug", Just Log.Debug) , ("none", Nothing)]
                          (long "log-level" <> value (Just Log.Warning) <> help "Log messages at or above this level, or disable logging entirely.")
      requestId <- optional (strOption $ long "request-id" <> help "A string to use as the request identifier for any logged messages." <> metavar "id")
      failOnWarning <- switch (long "fail-on-warning" <> help "Fail on assignment warnings.")
      pure $ Log.Options disableColour logLevel requestId False False Log.logfmtFormatter 0 failOnWarning

    argumentsParser = do
      subparser <- hsubparser (diffCommand <> parseCommand <>  tsParseCommand <> graphCommand)
      output <- ToPath <$> strOption (long "output" <> short 'o' <> help "Output path, defaults to stdout") <|> pure (ToHandle stdout)
      pure $ subparser >>= Task.write output

    diffCommand = command "diff" (info diffArgumentsParser (progDesc "Compute changes between paths"))
    diffArgumentsParser = do
      renderer <- flag  (SomeRenderer SExpressionDiffRenderer) (SomeRenderer SExpressionDiffRenderer) (long "sexpression" <> help "Output s-expression diff tree (default)")
              <|> flag'                                        (SomeRenderer JSONDiffRenderer)        (long "json"        <> help "Output JSON diff trees")
              <|> flag'                                        (SomeRenderer ToCDiffRenderer)         (long "toc"         <> help "Output JSON table of contents diff summary")
              <|> flag'                                        (SomeRenderer DOTDiffRenderer)         (long "dot"         <> help "Output the diff as a DOT graph")
      filesOrStdin <- Right <$> some (both <$> argument filePathReader (metavar "FILE_A") <*> argument filePathReader (metavar "FILE_B")) <|> pure (Left stdin)
      pure $ runDiff renderer filesOrStdin

    parseCommand = command "parse" (info parseArgumentsParser (progDesc "Generate parse trees for path(s)"))
    parseArgumentsParser = do
      renderer <- flag  (SomeRenderer SExpressionTermRenderer) (SomeRenderer SExpressionTermRenderer) (long "sexpression" <> help "Output s-expression parse trees (default)")
              <|> flag'                                        (SomeRenderer JSONTermRenderer)        (long "json"        <> help "Output JSON parse trees")
              <|> flag'                                        (SomeRenderer TagsTermRenderer)        (long "tags"        <> help "Output JSON tags")
              <|> flag'                                        (SomeRenderer . SymbolsTermRenderer)   (long "symbols"     <> help "Output JSON symbol list")
                   <*> (option symbolFieldsReader (  long "fields"
                                                 <> help "Comma delimited list of specific fields to return (symbols output only)."
                                                 <> metavar "FIELDS")
                  <|> pure defaultSymbolFields)
              <|> flag'                                        (SomeRenderer ImportsTermRenderer)     (long "import-graph" <> help "Output JSON import graph")
              <|> flag'                                        (SomeRenderer DOTTermRenderer)         (long "dot"          <> help "Output DOT graph parse trees")
      filesOrStdin <- Right <$> some (argument filePathReader (metavar "FILES...")) <|> pure (Left stdin)
      pure $ runParse renderer filesOrStdin

    tsParseCommand = command "ts-parse" (info tsParseArgumentsParser (progDesc "Print specialized tree-sitter ASTs for path(s)"))
    tsParseArgumentsParser = do
      renderer <- flag  (SomeRenderer SExpressionTermRenderer) (SomeRenderer SExpressionTermRenderer) (long "sexpression" <> help "Output s-expression ASTs (default)")
              <|> flag'                                        (SomeRenderer JSONTermRenderer)        (long "json"        <> help "Output JSON ASTs")
      filesOrStdin <- Right <$> some (argument filePathReader (metavar "FILES...")) <|> pure (Left stdin)
      pure $ runASTParse renderer filesOrStdin

    graphCommand = command "graph" (info graphArgumentsParser (progDesc "Compute a graph for a directory or entry point"))
    graphArgumentsParser = do
      graphType <- flag ImportGraph ImportGraph (long "imports" <> help "Compute an import graph (default)")
               <|> flag'            CallGraph   (long "calls"   <> help "Compute a call graph")
      serializer <- flag (Task.serialize (DOT style)) (Task.serialize (DOT style)) (long "dot"  <> help "Output in DOT graph format (default)")
                <|> flag'                             (Task.serialize JSON)        (long "json" <> help "Output JSON graph")
      rootDir <- rootDirectoryOption
      excludeDirs <- excludeDirsOption
      File{..} <- argument filePathReader (metavar "DIR:LANGUAGE | FILE")
      pure $ runGraph graphType rootDir filePath (fromJust fileLanguage) excludeDirs >>= fmap toOutput . serializer

    rootDirectoryOption = optional (strOption (long "root" <> help "Root directory of project. Optional, defaults to entry file/directory." <> metavar "DIR"))
    excludeDirsOption = many (strOption (long "exclude-dir" <> help "Exclude a directory (e.g. vendor)" <> metavar "DIR"))
    filePathReader = eitherReader parseFilePath
    parseFilePath arg = case splitWhen (== ':') arg of
        [a, b] | lang <- readMaybe b -> Right (File a lang)
               | lang <- readMaybe a -> Right (File b lang)
        [path] -> maybe (Left $ "Cannot identify language for path: " <> path) (Right . File path . Just) (languageForFilePath path)
        args -> Left ("cannot parse `" <> join args <> "`\nexpecting FILE:LANGUAGE or just FILE")

    optionsReader options = eitherReader $ \ str -> maybe (Left ("expected one of: " <> intercalate ", " (fmap fst options))) (Right . snd) (find ((== str) . fst) options)
    options options fields = option (optionsReader options) (fields <> showDefaultWith (findOption options) <> metavar (intercalate "|" (fmap fst options)))
    findOption options value = maybe "" fst (find ((== value) . snd) options)

    -- Example: semantic parse --symbols --fields=symbol,path,language,kind,line,span
    symbolFieldsReader = eitherReader parseSymbolFields
    parseSymbolFields arg = let fields = splitWhen (== ',') arg in
                      Right SymbolFields
                        { symbolFieldsName = "symbol" `elem` fields
                        , symbolFieldsPath = "path" `elem` fields
                        , symbolFieldsLang = "language" `elem` fields
                        , symbolFieldsKind = "kind" `elem` fields
                        , symbolFieldsLine = "line" `elem` fields
                        , symbolFieldsSpan = "span" `elem` fields
                        }
