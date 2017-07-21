{-# LANGUAGE TemplateHaskell #-}
module SemanticCmdLine
( main
-- Testing
, runDiff
, runParse
) where

import Files (languageForFilePath)
import Data.Functor.Both hiding (fst, snd)
import Data.List.Split (splitWhen)
import Data.Version (showVersion)
import Development.GitRev
import Language
import Options.Applicative hiding (action)
import Prologue hiding (concurrently, readFile)
import Renderer
import qualified Paths_semantic_diff as Library (version)
import qualified Semantic.Task as Task
import System.IO (stdin)
import qualified Semantic (parseBlobs, diffBlobPairs)

main :: IO ()
main = customExecParser (prefs showHelpOnEmpty) arguments >>= uncurry Task.runTaskWithOptions

runDiff :: SomeRenderer DiffRenderer -> Either Handle [Both (FilePath, Maybe Language)] -> Task.Task ByteString
runDiff (SomeRenderer diffRenderer) = Semantic.diffBlobPairs diffRenderer <=< Task.readBlobPairs

runParse :: SomeRenderer TermRenderer -> Either Handle [(FilePath, Maybe Language)] -> Task.Task ByteString
runParse (SomeRenderer parseTreeRenderer) = Semantic.parseBlobs parseTreeRenderer <=< Task.readBlobs

-- | A parser for the application's command-line arguments.
--
--   Returns a 'Task' to read the input, run the requested operation, and write the output to the specified output path or stdout.
arguments :: ParserInfo (Task.Options, Task.Task ())
arguments = info (version <*> helper <*> ((,) <$> optionsParser <*> argumentsParser)) description
  where
    version = infoOption versionString (long "version" <> short 'v' <> help "Output the version of the program")
    versionString = "semantic version " <> showVersion Library.version <> " (" <> $(gitHash) <> ")"
    description = fullDesc <> header "semantic -- Parse and diff semantically"

    optionsParser = Task.Options
      <$> options [("yes", Just True), ("no", Just False), ("auto", Nothing)]
            (long "colour" <> long "color" <> value Nothing <> help "Enable, disable, or decide automatically iff stderr is a terminal device, whether to use colour.")
      <*> pure (Just Task.Warning)
    argumentsParser = (. Task.writeToOutput) . (>>=)
      <$> hsubparser (diffCommand <> parseCommand)
      <*> (   Right <$> strOption (long "output" <> short 'o' <> help "Output path, defaults to stdout")
          <|> pure (Left stdout) )

    diffCommand = command "diff" (info diffArgumentsParser (progDesc "Show changes between commits or paths"))
    diffArgumentsParser = runDiff
      <$> (   flag  (SomeRenderer PatchDiffRenderer) (SomeRenderer PatchDiffRenderer)       (long "patch" <> help "Output a patch(1)-compatible diff (default)")
          <|> flag'                                  (SomeRenderer JSONDiffRenderer)        (long "json" <> help "Output a json diff")
          <|> flag'                                  (SomeRenderer SExpressionDiffRenderer) (long "sexpression" <> help "Output an s-expression diff tree")
          <|> flag'                                  (SomeRenderer ToCDiffRenderer)         (long "toc" <> help "Output a table of contents for a diff") )
      <*> (   ((Right . pure) .) . both
          <$> argument filePathReader (metavar "FILE_A")
          <*> argument filePathReader (metavar "FILE_B")
          <|> pure (Left stdin) )

    parseCommand = command "parse" (info parseArgumentsParser (progDesc "Print parse trees for path(s)"))
    parseArgumentsParser = runParse
      <$> (   flag  (SomeRenderer SExpressionTermRenderer) (SomeRenderer SExpressionTermRenderer) (long "sexpression" <> help "Output s-expression parse trees (default)")
          <|> flag'                                        (SomeRenderer JSONTermRenderer)        (long "json" <> help "Output JSON parse trees")
          <|> flag'                                        (SomeRenderer ToCTermRenderer)         (long "toc" <> help "Output a table of contents for a file"))
      <*> (   Right <$> some (argument filePathReader (metavar "FILES..."))
          <|> pure (Left stdin) )

    filePathReader = eitherReader parseFilePath
    parseFilePath arg = case splitWhen (== ':') arg of
        [a, b] | Just lang <- readMaybe a -> Right (b, Just lang)
               | Just lang <- readMaybe b -> Right (a, Just lang)
        [path] -> Right (path, languageForFilePath path)
        _ -> Left ("cannot parse `" <> arg <> "`\nexpecting LANGUAGE:FILE or just FILE")

    optionsReader options = eitherReader $ \ str -> maybe (Left ("expected one of: " <> intercalate ", " (fmap fst options))) (Right . snd) (find ((== str) . fst) options)
    options options fields = option (optionsReader options) (fields <> showDefaultWith (findOption options) <> metavar (intercalate "|" (fmap fst options)))
    findOption options value = maybe "" fst (find ((== value) . snd) options)
