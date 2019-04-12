{-# LANGUAGE RankNTypes #-}

module Data.File
  ( File (..)
  , file
  , toFile
  , readBlobFromFile
  , readBlobFromFile'
  , readBlobsFromDir
  , readBlobsFromGitRepo
  , readFilePair
  ) where

import qualified Control.Concurrent.Async as Async
import           Data.Blob
import qualified Data.ByteString as B
import           Data.Language
import           Data.Source
import           Prologue
import qualified Semantic.Git as Git
import           Semantic.IO
import           System.FilePath.Posix

data File = File
  { filePath     :: FilePath
  , fileLanguage :: Language
  } deriving (Eq, Ord, Show)

file :: FilePath -> File
file path = File path (languageForFilePath path)
  where languageForFilePath = languageForType . takeExtension

-- This is kind of a wart; Blob and File should be two views of
-- the same higher-kinded datatype.
toFile :: Blob -> File
toFile (Blob _ p l _) = File p l

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
  findFilesInDir path supportedExts mempty >>= Async.mapConcurrently (readBlobFromFile . file)

-- | Read all blobs from the Git repo with Language.supportedExts
readBlobsFromGitRepo :: MonadIO m => FilePath -> Git.OID -> m [Blob]
readBlobsFromGitRepo path oid = liftIO . fmap catMaybes $
  Git.lsTree path oid >>= Async.mapConcurrently (blobFromTreeEntry path)
  where
    -- Only read tree entries that are normal mode blobs in a language we can parse.
    blobFromTreeEntry :: FilePath -> Git.TreeEntry -> IO (Maybe Blob)
    blobFromTreeEntry gitDir (Git.TreeEntry Git.NormalMode Git.BlobObject oid path)
      | lang <- languageForFilePath path
      , lang `elem` codeNavLanguages
      = Just . sourceBlob' path lang oid . fromText <$> Git.catFile gitDir oid
    blobFromTreeEntry _ _ = pure Nothing

    sourceBlob' filepath language (Git.OID oid) source = Blob source filepath language oid

readFilePair :: forall m. (MonadFail m, MonadIO m) => File -> File -> m BlobPair
readFilePair a b = do
  before <- readBlobFromFile a
  after  <- readBlobFromFile b
  maybeBlobPair before after
