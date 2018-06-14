{-# OPTIONS_GHC -fforce-recomp #-} -- So that gitHash is correct.
{-# LANGUAGE ApplicativeDo, RankNTypes, TemplateHaskell #-}
module Semantic.CLI
( main
-- Testing
, Diff.runDiff
, Parse.runParse
) where

import           Data.Project
import           Data.Language (ensureLanguage)
import           Data.List (intercalate)
import           Data.List.Split (splitWhen)
import           Data.Version (showVersion)
import           Development.GitRev
import           Options.Applicative hiding (style)
import qualified Paths_semantic as Library (version)
import           Prologue
import           Rendering.Renderer
import qualified Semantic.AST as AST
import qualified Semantic.Diff as Diff
import qualified Semantic.Graph as Graph
import           Semantic.IO as IO
import qualified Semantic.Log as Log
import qualified Semantic.Parse as Parse
import qualified Semantic.Task as Task
import           Serializing.Format
import           Text.Read

main :: IO ()
main = customExecParser (prefs showHelpOnEmpty) arguments >>= uncurry Task.runTaskWithOptions

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
      renderer <- flag  (Diff.runDiff SExpressionDiffRenderer) (Diff.runDiff SExpressionDiffRenderer) (long "sexpression" <> help "Output s-expression diff tree (default)")
              <|> flag'                                        (Diff.runDiff JSONDiffRenderer)        (long "json"        <> help "Output JSON diff trees")
              <|> flag'                                        (Diff.runDiff ToCDiffRenderer)         (long "toc"         <> help "Output JSON table of contents diff summary")
              <|> flag'                                        (Diff.runDiff DOTDiffRenderer)         (long "dot"         <> help "Output the diff as a DOT graph")
              <|> flag'                                        (Diff.runDiff ShowDiffRenderer)        (long "show"        <> help "Output using the Show instance (debug only, format subject to change without notice)")
      filesOrStdin <- Right <$> some (both <$> argument filePathReader (metavar "FILE_A") <*> argument filePathReader (metavar "FILE_B")) <|> pure (Left stdin)
      pure $ Task.readBlobPairs filesOrStdin >>= renderer

    parseCommand = command "parse" (info parseArgumentsParser (progDesc "Generate parse trees for path(s)"))
    parseArgumentsParser = do
      renderer <- flag  (Parse.runParse SExpressionTermRenderer) (Parse.runParse SExpressionTermRenderer) (long "sexpression" <> help "Output s-expression parse trees (default)")
              <|> flag'                                          (Parse.runParse JSONTermRenderer)        (long "json"        <> help "Output JSON parse trees")
              <|> flag'                                          (Parse.runParse TagsTermRenderer)        (long "tags"        <> help "Output JSON tags")
              <|> flag'                                          (Parse.runParse . SymbolsTermRenderer)   (long "symbols"     <> help "Output JSON symbol list")
                   <*> (option symbolFieldsReader (  long "fields"
                                                 <> help "Comma delimited list of specific fields to return (symbols output only)."
                                                 <> metavar "FIELDS")
                  <|> pure defaultSymbolFields)
              <|> flag'                                          (Parse.runParse ImportsTermRenderer)     (long "import-graph" <> help "Output JSON import graph")
              <|> flag'                                          (Parse.runParse DOTTermRenderer)         (long "dot"          <> help "Output DOT graph parse trees")
              <|> flag'                                          (Parse.runParse ShowTermRenderer)        (long "show"         <> help "Output using the Show instance (debug only, format subject to change without notice)")
      filesOrStdin <- Right <$> some (argument filePathReader (metavar "FILES...")) <|> pure (Left stdin)
      pure $ Task.readBlobs filesOrStdin >>= renderer

    tsParseCommand = command "ts-parse" (info tsParseArgumentsParser (progDesc "Print specialized tree-sitter ASTs for path(s)"))
    tsParseArgumentsParser = do
      format <- flag  AST.SExpression AST.SExpression (long "sexpression" <> help "Output s-expression ASTs (default)")
            <|> flag'                 AST.JSON        (long "json"        <> help "Output JSON ASTs")
            <|> flag'                 AST.Show        (long "show"        <> help "Output using the Show instance (debug only, format subject to change without notice)")
      filesOrStdin <- Right <$> some (argument filePathReader (metavar "FILES...")) <|> pure (Left stdin)
      pure $ Task.readBlobs filesOrStdin >>= AST.runASTParse format

    graphCommand = command "graph" (info graphArgumentsParser (progDesc "Compute a graph for a directory or entry point"))
    graphArgumentsParser = do
      graphType <- flag  Graph.ImportGraph Graph.ImportGraph (long "imports" <> help "Compute an import graph (default)")
               <|> flag'                   Graph.CallGraph   (long "calls"   <> help "Compute a call graph")
      let style = Graph.style
      includePackages <- switch (long "packages" <> help "Include a vertex for the package, with edges from it to each module")
      serializer <- flag (Task.serialize (DOT style)) (Task.serialize (DOT style)) (long "dot"  <> help "Output in DOT graph format (default)")
                <|> flag'                             (Task.serialize JSON)        (long "json" <> help "Output JSON graph")
                <|> flag'                             (Task.serialize Show)        (long "show" <> help "Output using the Show instance (debug only, format subject to change without notice)")
      rootDir <- rootDirectoryOption
      excludeDirs <- excludeDirsOption
      File{..} <- argument filePathReader (metavar "DIR:LANGUAGE | FILE")
      pure $ Task.readProject rootDir filePath fileLanguage excludeDirs >>= Graph.runGraph graphType includePackages >>= serializer

    rootDirectoryOption = optional (strOption (long "root" <> help "Root directory of project. Optional, defaults to entry file/directory." <> metavar "DIR"))
    excludeDirsOption = many (strOption (long "exclude-dir" <> help "Exclude a directory (e.g. vendor)" <> metavar "DIR"))
    filePathReader = eitherReader parseFilePath
    parseFilePath arg = case splitWhen (== ':') arg of
        [a, b] | (Just lang) <- readMaybe b >>= ensureLanguage -> Right (File a lang)
               | (Just lang) <- readMaybe a >>= ensureLanguage -> Right (File b lang)
        [path] -> maybe (Left $ "Cannot identify language for path: " <> path) (Right . File path) (ensureLanguage (languageForFilePath path))
        args -> Left ("cannot parse `" <> join args <> "`\nexpecting FILE:LANGUAGE or just FILE")

    optionsReader options = eitherReader $ \ str -> maybe (Left ("expected one of: " <> intercalate ", " (fmap fst options))) (Right . snd) (find ((== str) . fst) options)
    options options fields = option (optionsReader options) (fields <> showDefaultWith (findOption options) <> metavar (intercalate "|" (fmap fst options)))
    findOption options value = maybe "" fst (find ((== value) . snd) options)

    -- Example: semantic parse --symbols --fields=symbol,path,language,kind,line,span
    symbolFieldsReader = eitherReader (Right . parseSymbolFields)
