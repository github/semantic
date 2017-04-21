{-# LANGUAGE TemplateHaskell #-}
module SemanticCmdLine (main, runDiff, runParse) where

import Arguments
import Command
import Data.Functor.Both
import Data.List.Split (splitWhen)
import Data.String
import Data.Version (showVersion)
import Development.GitRev
import Options.Applicative hiding (action)
import Prologue hiding (concurrently, fst, snd, readFile)
import qualified Data.ByteString as B
import qualified Paths_semantic_diff as Library (version)
import System.Directory
import System.Environment
import System.FilePath.Posix (takeFileName, (-<.>))
import System.IO.Error (IOError)
import Text.Regex
import qualified Semantic (parseBlobs, diffBlobs)

main :: IO ()
main = do
  gitDir <- findGitDir
  alternates <- findAlternates
  Arguments{..} <- customExecParser (prefs showHelpOnEmpty) (arguments gitDir alternates)
  outputPath <- getOutputPath outputFilePath
  text <- case programMode of
    Diff args -> runDiff args
    Parse args -> runParse args
  writeToOutput outputPath text
  where
    findGitDir = do
      pwd <- getCurrentDirectory
      fromMaybe pwd <$> lookupEnv "GIT_DIR"

    findAlternates = do
      eitherObjectDirs <- try $ splitWhen (== ':') . toS <$> getEnv "GIT_ALTERNATE_OBJECT_DIRECTORIES"
      pure $ case (eitherObjectDirs :: Either IOError [FilePath]) of
              (Left _) -> []
              (Right objectDirs) -> objectDirs

    getOutputPath Nothing = pure Nothing
    getOutputPath (Just path) = do
      isDir <- doesDirectoryExist path
      pure . Just $ if isDir then takeFileName path -<.> ".html" else path

    writeToOutput :: Maybe FilePath -> ByteString -> IO ()
    writeToOutput = maybe B.putStr B.writeFile

runDiff :: DiffArguments -> IO ByteString
runDiff DiffArguments{..} = do
  blobs <- runCommand $ case diffMode of
   DiffPaths a b -> pure <$> traverse readFile (both a b)
   DiffCommits sha1 sha2 paths -> readFilesAtSHAs gitDir alternateObjectDirs paths (both sha1 sha2)
  Semantic.diffBlobs diffRenderer blobs

runParse :: ParseArguments -> IO ByteString
runParse ParseArguments{..} = do
  blobs <- runCommand $ case parseMode of
    ParsePaths paths -> traverse readFile paths
    ParseCommit sha paths -> readFilesAtSHA gitDir alternateObjectDirs paths sha
  Semantic.parseBlobs parseTreeRenderer blobs

-- | A parser for the application's command-line arguments.
arguments :: FilePath -> [FilePath] -> ParserInfo Arguments
arguments gitDir alternates = info (version <*> helper <*> argumentsParser) description
  where
    version = infoOption versionString (long "version" <> short 'v' <> help "Output the version of the program")
    versionString = "semantic version " <> showVersion Library.version <> " (" <> $(gitHash) <> ")"
    description = fullDesc <> progDesc "Set the GIT_DIR environment variable to specify a different git repository. Set GIT_ALTERNATE_OBJECT_DIRECTORIES to specify location of alternates."
                           <> header "semantic -- Parse and diff semantically"

    argumentsParser = Arguments
      <$> hsubparser (diffCommand <> parseCommand)
      <*> optional (strOption (long "output" <> short 'o' <> help "Output path (directory for split diffs), defaults to stdout"))

    diffCommand = command "diff" (info diffArgumentsParser (progDesc "Show changes between commits or paths"))
    diffArgumentsParser = Diff
      <$> ( (  flag patchDiff patchDiff (long "patch" <> help "Output a patch(1)-compatible diff (default)")
           <|> flag' splitDiff (long "split" <> help "Output a split diff")
           <|> flag' jsonDiff (long "json" <> help "Output a json diff")
           <|> flag' summaryDiff (long "summary" <> help "Output a diff summary")
           <|> flag' sExpressionDiff (long "sexpression" <> help "Output an s-expression diff tree")
           <|> flag' tocDiff (long "toc" <> help "Output a table of contents diff summary") )
         <*> (  DiffPaths
               <$> argument str (metavar "FILE_A")
               <*> argument str (metavar "FILE_B")
            <|> DiffCommits
               <$> option (eitherReader parseSha) (long "sha1" <> metavar "SHA" <> help "Starting commit SHA")
               <*> option (eitherReader parseSha) (long "sha2" <> metavar "SHA" <> help "Ending commit SHA")
               <*> many (argument str (metavar "FILES...")) )
         <*> pure gitDir
         <*> pure alternates )

    parseCommand = command "parse" (info parseArgumentsParser (progDesc "Print parse trees for a commit or paths"))
    parseArgumentsParser = Parse
      <$> ( (  flag sExpressionParseTree sExpressionParseTree (long "sexpression" <> help "Output s-expression parse trees (default)")
           <|> flag' jsonParseTree (long "json" <> help "Output JSON parse trees")
           <|> flag' jsonIndexParseTree (long "index" <> help "Output JSON parse trees in index format") )
         <*> (  ParsePaths
               <$> some (argument str (metavar "FILES..."))
            <|> ParseCommit
               <$> option (eitherReader parseSha) (long "sha" <> metavar "SHA" <> help "Commit SHA")
               <*> some (argument str (metavar "FILES...")) )
         <*> switch (long "debug")
         <*> pure gitDir
         <*> pure alternates )

    parseSha :: String -> Either String String
    parseSha s = case matchRegex regex s of
      Just [sha] -> Right sha
      _ -> Left $ s <> " is not a valid SHA-1"
      where regex = mkRegexWithOpts "([0-9a-f]{40})" True False
