-- | These are primitive file IO methods for use in ghci and as internal functions.
-- Instead of using these, consider if you can use the Files DSL instead.
module Data.Blob.IO
  ( readBlobFromFile
  , readBlobFromFile'
  , readBlobsFromDir
  , readFilePair
  ) where

import Prologue

import qualified Control.Concurrent.Async as Async
import           Data.Blob
import qualified Data.ByteString as B
import           Data.Language
import           Semantic.IO
import qualified Source.Source as Source
import qualified System.Path as Path

-- | Read a utf8-encoded file to a 'Blob'.
readBlobFromFile :: MonadIO m => File -> m (Maybe Blob)
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
  findFilesInDir path supportedExts mempty >>= Async.mapConcurrently (readBlobFromFile . fileForTypedPath)

readFilePair :: MonadIO m => File -> File -> m BlobPair
readFilePair a b = do
  before <- readBlobFromFile a
  after  <- readBlobFromFile b
  liftIO $ maybeBlobPair before after
