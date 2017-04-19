module Command.Git (sourceBlobsFromSha) where

import Prologue
import Source
import Command.Files
import Data.String
import Git.Blob
import Git.Libgit2
import Git.Repository
import Git.Types
import qualified Git

-- | For the given sha, git repo path, and file paths, retrieves the source blobs.
sourceBlobsFromSha :: String -> String -> [FilePath] -> IO [SourceBlob]
sourceBlobsFromSha commitSha gitDir filePaths = do
  maybeBlobs <- withRepository lgFactory gitDir $ do
    repo   <- getRepository
    object <- parseObjOid (toS commitSha)
    commit <- lookupCommit object
    tree   <- lookupTree (commitTree commit)
    lift $ runReaderT (traverse (toSourceBlob tree) filePaths) repo

  pure $ catMaybes maybeBlobs

  where
    toSourceBlob :: Git.Tree LgRepo -> FilePath -> ReaderT LgRepo IO (Maybe SourceBlob)
    toSourceBlob tree filePath = do
      entry <- treeEntry tree (toS filePath)
      case entry of
        Just (BlobEntry entryOid entryKind) -> do
          blob <- lookupBlob entryOid
          bytestring <- blobToByteString blob
          let oid = renderObjOid $ blobOid blob
          s <- liftIO $ transcode bytestring
          pure . Just $ SourceBlob s (toS oid) filePath (Just (toSourceKind entryKind))
        _ -> pure Nothing
      where
        toSourceKind :: Git.BlobKind -> SourceKind
        toSourceKind (Git.PlainBlob mode) = Source.PlainBlob mode
        toSourceKind (Git.ExecutableBlob mode) = Source.ExecutableBlob mode
        toSourceKind (Git.SymlinkBlob mode) = Source.SymlinkBlob mode
