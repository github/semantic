{-# LANGUAGE RankNTypes #-}

-- | These are primitive file IO methods for use in ghci and as internal functions.
-- Instead of using these, consider if you can use the Files DSL instead.
module Data.Blob.IO
  ( readBlobFromFile
  , readBlobFromFile'
  , readBlobsFromDir
  , readBlobsFromGitRepo
  , readBlobsFromGitRepoPath
  , readFilePair
  ) where

import Prologue

import qualified Control.Concurrent.Async as Async
import           Data.Blob
import qualified Data.ByteString as B
import           Data.Language
import           Data.Text.Encoding (decodeUtf8)
import qualified Semantic.Git as Git
import           Semantic.IO
import qualified Source.Source as Source
import qualified System.Path as Path
import qualified System.Path.PartClass as Part

-- | Read a utf8-encoded file to a 'Blob'.
readBlobFromFile :: forall m. MonadIO m => File -> m (Maybe Blob)
readBlobFromFile (File "/dev/null" _) = pure Nothing
readBlobFromFile (File path language) = do
  raw <- liftIO $ B.readFile path
  pure . Just . sourceBlob path language . Source.fromUTF8 $ raw

-- | Read a utf8-encoded file to a 'Blob', raising an IOError if it can't be found.
readBlobFromFile' :: MonadIO m => File -> m Blob
readBlobFromFile' file = do
  maybeFile <- readBlobFromFile file
  maybeM (Prelude.fail ("cannot read '" <> show file <> "', file not found or language not supported.")) maybeFile

-- | Read all blobs in the directory with Language.supportedExts.
readBlobsFromDir :: MonadIO m => Path.AbsRelDir -> m [Blob]
readBlobsFromDir path = liftIO . fmap catMaybes $
  findFilesInDir (Path.toString path) supportedExts mempty >>= Async.mapConcurrently (readBlobFromFile . fileForPath)

readBlobsFromGitRepoPath :: (Part.AbsRel ar, MonadIO m) => Path.Dir ar -> Git.OID -> [Path.RelFile] -> [Path.RelFile] -> m [Blob]
readBlobsFromGitRepoPath path oid excludePaths includePaths
  = readBlobsFromGitRepo (Path.toString path) oid (fmap Path.toString excludePaths) (fmap Path.toString includePaths)

-- | Read all blobs from a git repo. Prefer readBlobsFromGitRepoPath, which is typed.
readBlobsFromGitRepo :: MonadIO m => FilePath -> Git.OID -> [FilePath] -> [FilePath] -> m [Blob]
readBlobsFromGitRepo path oid excludePaths includePaths = liftIO . fmap catMaybes $
  Git.lsTree path oid >>= Async.mapConcurrently (blobFromTreeEntry path)
  where
    -- Only read tree entries that are normal mode, non-minified blobs in a language we can parse.
    blobFromTreeEntry :: FilePath -> Git.TreeEntry -> IO (Maybe Blob)
    blobFromTreeEntry gitDir (Git.TreeEntry Git.NormalMode Git.BlobObject oid path)
      | lang <- languageForFilePath path
      , lang `elem` codeNavLanguages
      , not (pathIsMinified path)
      , path `notElem` excludePaths
      , null includePaths || path `elem` includePaths
      = Just . sourceBlob' path lang oid <$> Git.catFile gitDir oid
    blobFromTreeEntry _ _ = pure Nothing

    sourceBlob' filepath language (Git.OID oid) source = makeBlob source filepath language (decodeUtf8 oid)

readFilePair :: MonadIO m => File -> File -> m BlobPair
readFilePair a b = do
  before <- readBlobFromFile a
  after  <- readBlobFromFile b
  liftIO $ maybeBlobPair before after
