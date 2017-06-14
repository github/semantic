{-# LANGUAGE TemplateHaskell #-}
module SemanticCmdLine (main, runDiff, runParse) where

import Arguments
import Command
import Command.Files (languageForFilePath)
import Data.Functor.Both
import Data.List.Split (splitWhen)
import Data.Version (showVersion)
import Development.GitRev
import Options.Applicative hiding (action)
import Prologue hiding (concurrently, fst, snd, readFile)
import qualified Data.ByteString as B
import qualified Paths_semantic_diff as Library (version)
import qualified Semantic.Task as Task
import System.Directory
import System.FilePath.Posix (takeFileName, (-<.>))
import System.IO (stdin)
import qualified Semantic (parseBlobs, diffBlobPairs)

main :: IO ()
main = do
  Arguments{..} <- customExecParser (prefs showHelpOnEmpty) arguments
  outputPath <- traverse getOutputPath outputFilePath
  text <- case programMode of
    Diff args -> runDiff args
    Parse args -> runParse args
  writeToOutput outputPath text
  where
    getOutputPath path = do
      isDir <- doesDirectoryExist path
      pure $ if isDir then takeFileName path -<.> ".html" else path
    writeToOutput :: Maybe FilePath -> ByteString -> IO ()
    writeToOutput = maybe B.putStr B.writeFile

runDiff :: DiffArguments -> IO ByteString
runDiff DiffArguments{..} = do
  blobs <- runCommand $ case diffMode of
    DiffPaths a b -> pure <$> traverse (uncurry readFile) (both a b)
    DiffStdin -> readBlobPairsFromHandle stdin
  Task.runTask (Semantic.diffBlobPairs diffRenderer blobs)

runParse :: ParseArguments -> IO ByteString
runParse ParseArguments{..} = do
  blobs <- runCommand $ case parseMode of
    ParsePaths paths -> traverse (uncurry readFile) paths
    ParseStdin -> readBlobsFromHandle stdin
  Task.runTask (Semantic.parseBlobs parseTreeRenderer blobs)

-- | A parser for the application's command-line arguments.
arguments :: ParserInfo Arguments
arguments = info (version <*> helper <*> argumentsParser) description
  where
    version = infoOption versionString (long "version" <> short 'v' <> help "Output the version of the program")
    versionString = "semantic version " <> showVersion Library.version <> " (" <> $(gitHash) <> ")"
    description = fullDesc <> header "semantic -- Parse and diff semantically"

    argumentsParser = Arguments
      <$> hsubparser (diffCommand <> parseCommand)
      <*> optional (strOption (long "output" <> short 'o' <> help "Output path, defaults to stdout"))

    diffCommand = command "diff" (info diffArgumentsParser (progDesc "Show changes between commits or paths"))
    diffArgumentsParser = Diff
      <$> ( (  flag patchDiff patchDiff (long "patch" <> help "Output a patch(1)-compatible diff (default)")
           <|> flag' jsonDiff (long "json" <> help "Output a json diff")
           <|> flag' sExpressionDiff (long "sexpression" <> help "Output an s-expression diff tree")
           <|> flag' tocDiff (long "toc" <> help "Output a table of contents for a diff") )
         <*> (  DiffPaths
               <$> argument filePathReader (metavar "FILE_A")
               <*> argument filePathReader (metavar "FILE_B")
            <|> pure DiffStdin ))

    parseCommand = command "parse" (info parseArgumentsParser (progDesc "Print parse trees for path(s)"))
    parseArgumentsParser = Parse
      <$> ( (  flag sExpressionParseTree sExpressionParseTree (long "sexpression" <> help "Output s-expression parse trees (default)")
           <|> flag' jsonParseTree (long "json" <> help "Output JSON parse trees") )
         <*> (  ParsePaths
               <$> some (argument filePathReader (metavar "FILES..."))
            <|> pure ParseStdin ))

    filePathReader = eitherReader parseFilePath
    parseFilePath arg = case splitWhen (== ':') arg of
        [a, b] | Just lang <- readMaybe a -> Right (b, Just lang)
               | Just lang <- readMaybe b -> Right (a, Just lang)
        [path] -> Right (path, languageForFilePath path)
        _ -> Left ("cannot parse `" <> arg <> "`\nexpecting LANGUAGE:FILE or just FILE")
