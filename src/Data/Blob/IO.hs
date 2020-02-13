{-# LANGUAGE ViewPatterns #-}
-- | These are primitive file IO methods for use in ghci and as internal functions.
-- Instead of using these, consider if you can use the Files DSL instead.
module Data.Blob.IO
  ( readBlobFromFile
  , readBlobFromFile'
  , readBlobFromPath
  , readBlobsFromDir
  , readFilePair
  , readProjectFromPaths
  ) where

import           Analysis.Blob
import           Analysis.File as File
import           Analysis.Project
import qualified Control.Concurrent.Async as Async
import           Control.Monad.IO.Class
import           Data.Blob
import qualified Data.ByteString as B
import           Data.Language
import           Data.Maybe.Exts
import           Data.Semilattice.Lower
import           Semantic.IO
import qualified Source.Source as Source
import qualified System.Path as Path

-- | Deprecated: this has very weird semantics.
readProjectFromPaths :: MonadIO m
                     => Maybe Path.AbsRelDir -- ^ An optional root directory for the project
                     -> Path.AbsRelFileDir   -- ^ A file or directory to parse. Passing a file path loads all files in that file's parent directory.
                     -> Language
                     -> [Path.AbsRelDir]     -- ^ Directories to exclude.
                     -> m Project
readProjectFromPaths maybeRoot path lang excludeDirs = do
  let rootDir :: Path.AbsRelDir
      rootDir = case maybeRoot >>= Path.fromAbsRel of
        -- If we were provided a root directory, use that.
        Just root -> root
        Nothing   -> case Path.fileFromFileDir path of
          -- If we weren't and the path is a file, drop its file name.
          Just fp -> Path.takeDirectory fp
          -- Otherwise, load from the path.
          Nothing -> Path.dirFromFileDir path

  paths <- liftIO $ findFilesInDir rootDir exts excludeDirs
  blobs <- liftIO $ traverse (readBlobFromFile' . toFile) paths
  pure $ Project (Path.toString rootDir) blobs lang (fmap Path.toString excludeDirs)
  where
    toFile path = File path lowerBound lang
    exts = extensionsForLanguage lang


-- | Read a utf8-encoded file to a 'Blob'.
readBlobFromFile :: MonadIO m => File Language -> m (Maybe Blob)
readBlobFromFile (File (Path.toString -> "/dev/null") _ _) = pure Nothing
readBlobFromFile file@(File path _ _language) = do
  raw <- liftIO $ B.readFile (Path.toString path)
  let newblob = Blob (Source.fromUTF8 raw) file
  pure . Just $ newblob

-- | Read a utf8-encoded file to a 'Blob', failing if it can't be found.
readBlobFromFile' :: (MonadFail m, MonadIO m) => File Language -> m Blob
readBlobFromFile' file = do
  maybeFile <- readBlobFromFile file
  maybeM (fail ("cannot read '" <> show file <> "', file not found or language not supported.")) maybeFile

-- | Read a blob from the provided absolute or relative path , failing if it can't be found.
readBlobFromPath :: (MonadFail m, MonadIO m) => Path.AbsRelFile -> m Blob
readBlobFromPath = readBlobFromFile' . File.fromPath

-- | Read all blobs in the directory with Language.supportedExts.
readBlobsFromDir :: MonadIO m => Path.AbsRelDir -> m [Blob]
readBlobsFromDir path = liftIO . fmap catMaybes $
  findFilesInDir path supportedExts mempty >>= Async.mapConcurrently (readBlobFromFile . File.fromPath)

readFilePair :: MonadIO m => File Language -> File Language -> m BlobPair
readFilePair a b = do
  before <- readBlobFromFile a
  after  <- readBlobFromFile b
  liftIO $ maybeBlobPair before after
