{-# LANGUAGE TemplateHaskell #-}
module SemanticCmdLine
( main
-- Testing
, runDiff
, runParse
) where

import Command.Files (languageForFilePath, readBlobs, readBlobPairs)
import Data.Functor.Both
import Data.List.Split (splitWhen)
import Data.Version (showVersion)
import Development.GitRev
import Language
import Options.Applicative hiding (action)
import Prologue hiding (concurrently, fst, snd, readFile)
import Renderer
import qualified Data.ByteString as B
import qualified Paths_semantic_diff as Library (version)
import qualified Semantic.Task as Task
import System.Directory
import System.FilePath.Posix (takeFileName, (-<.>))
import System.IO (stdin)
import qualified Semantic (parseBlobs, diffBlobPairs)

main :: IO ()
main = do
  (command, outputFilePath) <- customExecParser (prefs showHelpOnEmpty) arguments
  outputPath <- traverse getOutputPath outputFilePath
  text <- command
  writeToOutput outputPath text
  where
    getOutputPath path = do
      isDir <- doesDirectoryExist path
      pure $ if isDir then takeFileName path -<.> ".html" else path
    writeToOutput :: Maybe FilePath -> ByteString -> IO ()
    writeToOutput = maybe B.putStr B.writeFile

runDiff :: SomeRenderer DiffRenderer -> Either Handle [Both (FilePath, Maybe Language)] -> IO ByteString
runDiff (SomeRenderer diffRenderer) from = do
  blobs <- readBlobPairs from
  Task.runTask (Semantic.diffBlobPairs diffRenderer blobs)

data ParseMode = ParseStdin | ParsePaths [(FilePath, Maybe Language)]
  deriving Show

runParse :: SomeRenderer TermRenderer -> Either Handle [(FilePath, Maybe Language)] -> IO ByteString
runParse (SomeRenderer parseTreeRenderer) from = do
  blobs <- readBlobs from
  Task.runTask (Semantic.parseBlobs parseTreeRenderer blobs)

-- | A parser for the application's command-line arguments.
--
--   Returns an 'IO' action producing 'ByteString' output, and a 'Maybe FilePath' to write the output to.
arguments :: ParserInfo (IO ByteString, Maybe FilePath)
arguments = info (version <*> helper <*> argumentsParser) description
  where
    version = infoOption versionString (long "version" <> short 'v' <> help "Output the version of the program")
    versionString = "semantic version " <> showVersion Library.version <> " (" <> $(gitHash) <> ")"
    description = fullDesc <> header "semantic -- Parse and diff semantically"

    argumentsParser = (,)
      <$> hsubparser (diffCommand <> parseCommand)
      <*> optional (strOption (long "output" <> short 'o' <> help "Output path, defaults to stdout"))

    diffCommand = command "diff" (info diffArgumentsParser (progDesc "Show changes between commits or paths"))
    diffArgumentsParser = runDiff
      <$> (   flag  (SomeRenderer PatchDiffRenderer) (SomeRenderer PatchDiffRenderer) (long "patch" <> help "Output a patch(1)-compatible diff (default)")
          <|> flag' (SomeRenderer JSONDiffRenderer)                                   (long "json" <> help "Output a json diff")
          <|> flag' (SomeRenderer SExpressionDiffRenderer)                            (long "sexpression" <> help "Output an s-expression diff tree")
          <|> flag' (SomeRenderer ToCDiffRenderer)                                    (long "toc" <> help "Output a table of contents for a diff") )
      <*> (   ((Right . pure) .) . both
          <$> argument filePathReader (metavar "FILE_A")
          <*> argument filePathReader (metavar "FILE_B")
          <|> pure (Left stdin) )

    parseCommand = command "parse" (info parseArgumentsParser (progDesc "Print parse trees for path(s)"))
    parseArgumentsParser = runParse
      <$> (   flag  (SomeRenderer SExpressionTermRenderer) (SomeRenderer SExpressionTermRenderer) (long "sexpression" <> help "Output s-expression parse trees (default)")
          <|> flag' (SomeRenderer JSONTermRenderer)                                               (long "json" <> help "Output JSON parse trees")
          <|> flag' (SomeRenderer ToCTermRenderer)                                                (long "toc" <> help "Output a table of contents for a file"))
      <*> (   Right <$> some (argument filePathReader (metavar "FILES..."))
          <|> pure (Left stdin) )

    filePathReader = eitherReader parseFilePath
    parseFilePath arg = case splitWhen (== ':') arg of
        [a, b] | Just lang <- readMaybe a -> Right (b, Just lang)
               | Just lang <- readMaybe b -> Right (a, Just lang)
        [path] -> Right (path, languageForFilePath path)
        _ -> Left ("cannot parse `" <> arg <> "`\nexpecting LANGUAGE:FILE or just FILE")
