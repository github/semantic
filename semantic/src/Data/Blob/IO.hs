{-# LANGUAGE ViewPatterns #-}
-- | These are primitive file IO methods for use in ghci and as internal functions.
-- Instead of using these, consider if you can use the Files DSL instead.
module Data.Blob.IO
  ( readBlobFromFile
  , readBlobFromFile'
  , readBlobFromPath
  , readFilePair
  , readProjectFromPaths
  ) where

import           Analysis.Blob
import           Analysis.File as File
import           Analysis.Project
import           Analysis.Reference
import           Control.Monad.IO.Class
import           Data.Blob
import qualified Data.ByteString as B
import           Data.Maybe.Exts
import           Semantic.IO
import           Source.Language
import qualified Source.Source as Source
import           Source.Span

-- | Deprecated: this has very weird semantics.
readProjectFromPaths :: MonadIO m
                     => Maybe FilePath -- ^ An optional root directory for the project
                     -> FilePath       -- ^ A file or directory to parse. Passing a file path loads all files in that file's parent directory.
                     -> Language
                     -> [FilePath]     -- ^ Directories to exclude.
                     -> m Project
readProjectFromPaths maybeRoot path lang excludeDirs = do
  let rootDir = fromMaybe path maybeRoot

  paths <- liftIO $ findFilesInDir rootDir exts excludeDirs
  blobs <- liftIO $ traverse (readBlobFromFile' . toFile) paths
  pure $ Project rootDir blobs lang excludeDirs
  where
    toFile path = File (Reference path (point (Pos 1 1))) lang
    exts = extensionsForLanguage lang


-- | Read a utf8-encoded file to a 'Blob'.
readBlobFromFile :: MonadIO m => File Language -> m (Maybe Blob)
readBlobFromFile (File (Reference "/dev/null" _) _) = pure Nothing
readBlobFromFile file@(File (Reference path _) _language) = do
  raw <- liftIO $ B.readFile path
  let newblob = Blob (Source.fromUTF8 raw) file
  pure . Just $ newblob

-- | Read a utf8-encoded file to a 'Blob', failing if it can't be found.
readBlobFromFile' :: (MonadFail m, MonadIO m) => File Language -> m Blob
readBlobFromFile' file = do
  maybeFile <- readBlobFromFile file
  maybeM (fail ("cannot read '" <> show file <> "', file not found or language not supported.")) maybeFile

-- | Read a blob from the provided absolute or relative path , failing if it can't be found.
readBlobFromPath :: (MonadFail m, MonadIO m) => FilePath -> m Blob
readBlobFromPath = readBlobFromFile' . File.fromPath

readFilePair :: MonadIO m => File Language -> File Language -> m BlobPair
readFilePair a b = do
  before <- readBlobFromFile a
  after  <- readBlobFromFile b
  liftIO $ maybeBlobPair before after
