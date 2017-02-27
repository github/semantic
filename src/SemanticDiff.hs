{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-incomplete-patterns #-}
module SemanticDiff (main, fetchDiff, fetchDiffs) where

import Arguments
import Prologue hiding (fst, snd)
import Data.String
import Data.Functor.Both
import Data.Version (showVersion)
import Text.Regex
import Diffing
import Git.Libgit2
import Git.Repository
import Git.Blob
import Git.Types
import Git.Libgit2.Backend
import Options.Applicative hiding (action)
import System.Timeout as Timeout
import System.FilePath.Posix (hasExtension)
import Data.List ((\\))
import qualified Diffing as D
import qualified Git
import qualified Paths_semantic_diff as Library (version)
import qualified Renderer as R
import qualified Source
import qualified Control.Concurrent.Async.Pool as Async
import GHC.Conc (numCapabilities)
import Development.GitRev
import Parse

main :: IO ()
main = do
  args@Arguments{..} <- programArguments =<< execParser argumentsParser
  case runMode of
    Diff -> runDiff args
    Parse -> Parse.run args

runDiff :: Arguments -> IO ()
runDiff args@Arguments{..} = case diffMode of
    PathDiff paths -> diffPaths args paths
    CommitDiff -> diffCommits args

-- | A parser for the application's command-line arguments.
argumentsParser :: ParserInfo CmdLineOptions
argumentsParser = info (version <*> helper <*> argumentsP)
                       (fullDesc <> progDesc "Set the GIT_DIR environment variable to specify the git repository. Set GIT_ALTERNATE_OBJECT_DIRECTORIES to specify location of alternates."
                                 <> header "semantic-diff - Show semantic changes between commits")
  where
    argumentsP :: Parser CmdLineOptions
    argumentsP = CmdLineOptions
      <$> (flag R.Split R.Patch (long "patch" <> help "output a patch(1)-compatible diff")
      <|> flag R.Split R.JSON (long "json" <> help "output a json diff")
      <|> flag' R.Split (long "split" <> help "output a split diff")
      <|> flag' R.Summary (long "summary" <> help "output a diff summary")
      <|> flag' R.SExpression (long "sexpression" <> help "output an s-expression diff tree")
      <|> flag' R.TOC (long "toc" <> help "output a table of contents diff summary"))
      <*> optional (option auto (long "timeout" <> help "timeout for per-file diffs in seconds, defaults to 7 seconds"))
      <*> optional (strOption (long "output" <> short 'o' <> help "output directory for split diffs, defaults to stdout if unspecified"))
      <*> switch (long "no-index" <> help "compare two paths on the filesystem")
      <*> some (argument (eitherReader parseShasAndFiles) (metavar "SHA_A..SHAB FILES..."))
      <*> switch (long "development" <> short 'd' <> help "set development mode which prevents timeout behavior by default")
      <*> flag Diff Parse (long "parse" <> short 'p' <> help "parses a source file without diffing")
      where
        parseShasAndFiles :: String -> Either String ExtraArg
        parseShasAndFiles s = case matchRegex regex s of
          Just ["", sha2] -> Right . ShaPair $ both Nothing (Just sha2)
          Just [sha1, sha2] -> Right . ShaPair $ Just <$> both sha1 sha2
          _ -> Right $ FileArg s
          where regex = mkRegexWithOpts "([0-9a-f]{40})\\.\\.([0-9a-f]{40})" True False

versionString :: String
versionString = "semantic-diff version " <> showVersion Library.version <> " (" <> $(gitHash) <> ")"

version :: Parser (a -> a)
version = infoOption versionString (long "version" <> short 'V' <> help "output the version of the program")

-- | Compare changes between two commits.
diffCommits :: Arguments -> IO ()
diffCommits args@Arguments{..} = do
  ts <- fetchTerms args
  writeToOutput output (maybe mempty R.concatOutputs ts)
  where fetchTerms args = if developmentMode
                            then Just <$> fetchDiffs args
                            else Timeout.timeout timeoutInMicroseconds (fetchDiffs args)


-- | Compare two paths on the filesystem (compariable to git diff --no-index).
diffPaths :: Arguments -> Both FilePath -> IO ()
diffPaths args@Arguments{..} paths = do
  sources <- traverse readAndTranscodeFile paths
  let sourceBlobs = Source.SourceBlob <$> sources <*> pure mempty <*> paths <*> pure (Just Source.defaultPlainBlob)
  D.printDiff (parserForFilepath path) (diffArgs args) sourceBlobs
  where
    diffArgs Arguments{..} = R.DiffArguments { format = format, output = output }
    path = fromMaybe (panic "none of the paths have file extensions") $ find hasExtension paths

fetchDiffs :: Arguments -> IO [R.Output]
fetchDiffs args@Arguments{..} = do
  paths <- case(filePaths, shaRange) of
    ([], Join (Just a, Just b)) -> pathsToDiff args (both a b)
    (ps, _) -> pure ps

  Async.withTaskGroup numCapabilities $ \p ->
    Async.mapTasks p (fetchDiff args <$> paths)

fetchDiff :: Arguments -> FilePath -> IO R.Output
fetchDiff args@Arguments{..} filepath = withRepository lgFactory gitDir $ do
  repo <- getRepository
  for_ alternateObjectDirs (liftIO . odbBackendAddPath repo . toS)
  lift $ runReaderT (fetchDiff' args filepath) repo

fetchDiff' :: Arguments -> FilePath -> ReaderT LgRepo IO R.Output
fetchDiff' Arguments{..} filepath = do
  sourcesAndOids <- sequence $ traverse (getSourceBlob filepath) <$> shaRange

  let sources = fromMaybe (Source.emptySourceBlob filepath) <$> sourcesAndOids
  let sourceBlobs = Source.idOrEmptySourceBlob <$> sources
  let textDiff = D.textDiff (parserForFilepath filepath) diffArguments sourceBlobs

  text <- fetchText textDiff
  truncatedPatch <- liftIO $ D.truncatedDiff diffArguments sourceBlobs
  pure $ fromMaybe truncatedPatch text
  where
    diffArguments = R.DiffArguments { format = format, output = output }
    fetchText textDiff = if developmentMode
                          then liftIO $ Just <$> textDiff
                          else liftIO $ Timeout.timeout timeoutInMicroseconds textDiff


pathsToDiff :: Arguments -> Both String -> IO [FilePath]
pathsToDiff Arguments{..} shas = withRepository lgFactory gitDir $ do
  repo <- getRepository
  for_ alternateObjectDirs (liftIO . odbBackendAddPath repo . toS)
  lift $ runReaderT (pathsToDiff' shas) repo

-- | Returns a list of relative file paths that have changed between the given commit shas.
pathsToDiff' :: Both String -> ReaderT LgRepo IO [FilePath]
pathsToDiff' shas = do
  entries <- blobEntriesToDiff shas
  pure $ (\(p, _, _) -> toS p) <$> entries

-- | Returns a list of blob entries that have changed between the given commits shas.
blobEntriesToDiff :: Both String -> ReaderT LgRepo IO [(TreeFilePath, Git.BlobOid LgRepo, BlobKind)]
blobEntriesToDiff shas = do
  a <- blobEntries (fst shas)
  b <- blobEntries (snd shas)
  pure $ (a \\ b) <> (b \\ a)
  where blobEntries sha = treeForCommitSha sha >>= treeBlobEntries

-- | Returns a Git.Tree for a commit sha
treeForCommitSha :: String -> ReaderT LgRepo IO (Git.Tree LgRepo)
treeForCommitSha sha = do
  object <- parseObjOid (toS sha)
  commit <- lookupCommit object
  lookupTree (commitTree commit)

-- | Returns a SourceBlob given a relative file path, and the sha to look up.
getSourceBlob :: FilePath -> String -> ReaderT LgRepo IO Source.SourceBlob
getSourceBlob path sha = do
  tree <- treeForCommitSha sha
  entry <- treeEntry tree (toS path)
  (bytestring, oid, mode) <- case entry of
    Nothing -> pure (mempty, mempty, Nothing)
    Just (BlobEntry entryOid entryKind) -> do
      blob <- lookupBlob entryOid
      s <- blobToByteString blob
      let oid = renderObjOid $ blobOid blob
      pure (s, oid, Just entryKind)
  s <- liftIO $ transcode bytestring
  pure $ Source.SourceBlob s (toS oid) path (toSourceKind <$> mode)
  where
    toSourceKind :: Git.BlobKind -> Source.SourceKind
    toSourceKind (Git.PlainBlob mode) = Source.PlainBlob mode
    toSourceKind (Git.ExecutableBlob mode) = Source.ExecutableBlob mode
    toSourceKind (Git.SymlinkBlob mode) = Source.SymlinkBlob mode
