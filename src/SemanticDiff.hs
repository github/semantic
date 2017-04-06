{-# LANGUAGE TemplateHaskell #-}
module SemanticDiff (main) where

import Arguments
import Command
import Command.Parse
import Data.Aeson
import Data.Functor.Both
import Data.List.Split (splitWhen)
import Data.String
import Data.Version (showVersion)
import Development.GitRev
import Options.Applicative hiding (action)
import Prologue hiding (concurrently, fst, snd, readFile)
import qualified Data.ByteString as B
import qualified Paths_semantic_diff as Library (version)
import qualified Renderer as R
import qualified Renderer.SExpression as R
import Source
import System.Directory
import System.Environment
import System.FilePath.Posix (takeFileName, (-<.>))
import System.IO.Error (IOError)
import Text.Regex

main :: IO ()
main = do
  gitDir <- findGitDir
  alternates <- findAlternates
  Arguments{..} <- customExecParser (prefs showHelpOnEmpty) (arguments gitDir alternates)
  text <- case programMode of
    Diff DiffArguments{..} -> runCommand $ do
      let render = case diffFormat of
            R.Split -> fmap encodeText . renderDiffs R.SplitRenderer
            R.Patch -> fmap encodeText . renderDiffs R.PatchRenderer
            R.JSON -> fmap encodeJSON . renderDiffs R.JSONDiffRenderer
            R.Summary -> fmap encodeSummaries . renderDiffs R.SummaryRenderer
            R.SExpression -> renderDiffs (R.SExpressionDiffRenderer R.TreeOnly)
            R.TOC -> fmap encodeSummaries . renderDiffs R.ToCRenderer
      diffs <- case diffMode of
        DiffPaths pathA pathB -> do
          let paths = both pathA pathB
          blobs <- traverse readFile paths
          terms <- traverse (traverse parseBlob) blobs
          diff' <- maybeDiff terms
          pure [(fromMaybe . emptySourceBlob <$> paths <*> blobs, diff')]
        DiffCommits sha1 sha2 paths -> do
          blobPairs <- readFilesAtSHAs gitDir alternateObjectDirs paths (both sha1 sha2)
          concurrently blobPairs . uncurry $ \ path blobs -> do
            terms <- concurrently blobs (traverse parseBlob)
            diff' <- maybeDiff terms
            pure (fromMaybe <$> pure (emptySourceBlob path) <*> blobs, diff')
      render (diffs >>= \ (blobs, diff) -> (,) blobs <$> toList diff)
    Parse args'@ParseArguments{..} -> case parseFormat of
      R.JSONTree -> parseTree args'
      R.JSONIndex -> parseIndex args'
      R.SExpressionTree -> parseSExpression args'
  outputPath <- getOutputPath outputFilePath
  writeToOutput outputPath text

  where
    encodeText = encodeUtf8 . R.unFile
    encodeJSON = toS . (<> "\n") . encode
    encodeSummaries = toS . (<> "\n") . encode

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

-- | A parser for the application's command-line arguments.
arguments :: FilePath -> [FilePath] -> ParserInfo Arguments
arguments gitDir alternates = info (version <*> helper <*> argumentsParser) description
  where
    version = infoOption versionString (long "version" <> short 'v' <> help "Output the version of the program")
    versionString = "semantic-diff version " <> showVersion Library.version <> " (" <> $(gitHash) <> ")"
    description = fullDesc <> progDesc "Set the GIT_DIR environment variable to specify a different git repository. Set GIT_ALTERNATE_OBJECT_DIRECTORIES to specify location of alternates."
                           <> header "semantic -- Parse and diff semantically"

    argumentsParser = Arguments
      <$> hsubparser (diffCommand <> parseCommand)
      <*> optional (strOption (long "output" <> short 'o' <> help "Output path (directory for split diffs), defaults to stdout"))

    diffCommand = command "diff" (info diffArgumentsParser (progDesc "Show changes between commits or paths"))
    diffArgumentsParser = Diff
      <$> ( DiffArguments
            <$> (  flag R.Patch R.Patch (long "patch" <> help "Output a patch(1)-compatible diff (default)")
               <|> flag' R.JSON (long "json" <> help "Output a json diff")
               <|> flag' R.Split (long "split" <> help "Output a split diff")
               <|> flag' R.Summary (long "summary" <> help "Output a diff summary")
               <|> flag' R.SExpression (long "sexpression" <> help "Output an s-expression diff tree")
               <|> flag' R.TOC (long "toc" <> help "Output a table of contents diff summary") )
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
      <$> ( ParseArguments
            <$> (  flag R.SExpressionTree R.SExpressionTree (long "sexpression" <> help "Output s-expression parse trees (default)")
               <|> flag' R.JSONTree (long "json" <> help "Output JSON parse trees")
               <|> flag' R.JSONIndex (long "index" <> help "Output JSON parse trees in index format") )
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
