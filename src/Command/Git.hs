module Command.Git
( readFilesAtSHA
, readFilesAtSHAs
) where

import qualified Control.Concurrent.Async as Async
import Data.Functor.Both
import Data.String
import Data.List ((\\), nub)
import Prologue
import Git.Blob
import Git.Libgit2
import Git.Libgit2.Backend
import Git.Repository
import Git.Types
import qualified Git
import GitmonClient
import Command.Files
import Language
import Source

-- | Read files at the specified commit SHA as blobs from a Git repo.
readFilesAtSHA :: FilePath -> [FilePath] -> [(FilePath, Maybe Language)] -> String -> IO [SourceBlob]
readFilesAtSHA gitDir alternates paths sha = runGit gitDir alternates $ do
  tree <- treeForSha sha
  traverse (uncurry (blobForPathInTree tree)) paths

-- | Read files at the specified commit SHA pair as blobs from a Git repo.
readFilesAtSHAs :: FilePath -> [FilePath] -> [(FilePath, Maybe Language)] -> Both String -> IO [Both SourceBlob]
readFilesAtSHAs gitDir alternates paths shas = do
  paths <- case paths of
    [] -> runGit' $ do
      trees <- for shas treeForSha
      paths <- for trees (reportGitmon "ls-tree" . treeBlobEntries)
      pure . nub $! (\ (p, _, _) -> (toS p, languageForFilePath (toS p))) <$> runBothWith (\\) paths <> runBothWith (flip (\\)) paths
    _ -> pure paths

  Async.mapConcurrently (runGit' . blobsForPath) paths
  where
    runGit' = runGit gitDir alternates
    blobsForPath (path, lang) = do
      trees <- traverse treeForSha shas
      traverse (\t -> blobForPathInTree t path lang) trees

runGit :: FilePath -> [FilePath] -> ReaderT LgRepo IO a -> IO a
runGit gitDir alternates action = withRepository lgFactory gitDir $ do
  repo <- getRepository
  for_ alternates (liftIO . odbBackendAddPath repo . toS)
  action

treeForSha :: String -> ReaderT LgRepo IO (Git.Tree LgRepo)
treeForSha sha = do
  obj <- parseObjOid (toS sha)
  commit <- reportGitmon "cat-file" $ lookupCommit obj
  reportGitmon "cat-file" $ lookupTree (commitTree commit)

blobForPathInTree :: Git.Tree LgRepo -> FilePath -> Maybe Language -> ReaderT LgRepo IO SourceBlob
blobForPathInTree tree path language = do
  entry <- reportGitmon "ls-tree" $ treeEntry tree (toS path)
  case entry of
    Just (BlobEntry entryOid entryKind) -> do
      blob <- reportGitmon "cat-file" $ lookupBlob entryOid
      contents <- blobToByteString blob
      let oid = renderObjOid $ blobOid blob
      pure (SourceBlob (Source contents) (toS oid) path (Just (toSourceKind entryKind)) language)
    _ -> pure (emptySourceBlob path)
  where
    toSourceKind :: Git.BlobKind -> SourceKind
    toSourceKind (Git.PlainBlob mode) = Source.PlainBlob mode
    toSourceKind (Git.ExecutableBlob mode) = Source.ExecutableBlob mode
    toSourceKind (Git.SymlinkBlob mode) = Source.SymlinkBlob mode
