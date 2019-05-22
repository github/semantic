{-# LANGUAGE Rank2Types #-}

-- | These are primitive file IO methods for use in ghci and as internal functions.
-- Instead of using these, consider if you can use the Files DSL instead.
module Data.Blob.IO
  ( readBlobFromFile
  , readBlobFromFile'
  , readBlobsFromDir
  , readBlobsFromGitRepo
  , readFilePair
  ) where

import Prologue

import Data.Blob
import Data.Language
import Semantic.IO
import Data.Source
import qualified Semantic.Git as Git
import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString as B

-- | Read a utf8-encoded file to a 'Blob'.
readBlobFromFile :: forall m. MonadIO m => File -> m (Maybe Blob)
readBlobFromFile (File "/dev/null" _) = pure Nothing
readBlobFromFile (File path language) = do
  raw <- liftIO $ B.readFile path
  pure . Just . sourceBlob path language . fromUTF8 $ raw

-- | Read a utf8-encoded file to a 'Blob', raising an IOError if it can't be found.
readBlobFromFile' :: MonadIO m => File -> m Blob
readBlobFromFile' file = do
  maybeFile <- readBlobFromFile file
  maybeM (Prelude.fail ("cannot read '" <> show file <> "', file not found or language not supported.")) maybeFile

-- | Read all blobs in the directory with Language.supportedExts
readBlobsFromDir :: MonadIO m => FilePath -> m [Blob]
readBlobsFromDir path = liftIO . fmap catMaybes $
  findFilesInDir path supportedExts mempty >>= Async.mapConcurrently (readBlobFromFile . fileForPath)

-- | Read all blobs from the Git repo with Language.supportedExts
readBlobsFromGitRepo :: MonadIO m => FilePath -> Git.OID -> [FilePath] -> m [Blob]
readBlobsFromGitRepo path oid excludePaths = liftIO . fmap catMaybes $
  Git.lsTree path oid >>= Async.mapConcurrently (blobFromTreeEntry path)
  where
    -- Only read tree entries that are normal mode, non-minified blobs in a language we can parse.
    blobFromTreeEntry :: FilePath -> Git.TreeEntry -> IO (Maybe Blob)
    blobFromTreeEntry gitDir (Git.TreeEntry Git.NormalMode Git.BlobObject oid path)
      | lang <- languageForFilePath path
      , lang `elem` codeNavLanguages
      , not (pathIsMinified path)
      , path `notElem` excludePaths
      = Just . sourceBlob' path lang oid . fromText <$> Git.catFile gitDir oid
    blobFromTreeEntry _ _ = pure Nothing

    sourceBlob' filepath language (Git.OID oid) source = legacyMakeBlob source filepath language oid

readFilePair :: MonadIO m => File -> File -> m BlobPair
readFilePair a b = do
  before <- readBlobFromFile a
  after  <- readBlobFromFile b
  liftIO $ maybeBlobPair before after
