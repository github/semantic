{-# LANGUAGE RankNTypes #-}

module Data.File
  ( File (..)
  , file
  , toFile
  , readBlobFromFile
  , readBlobFromFile'
  , readBlobsFromDir
  , readFilePair
  , maybeThese
  ) where

import Prologue

import qualified Data.ByteString as B
import           System.FilePath.Glob
import           System.FilePath.Posix

import Data.Blob
import Data.Language
import Data.Source

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
toFile (Blob _ p l) = File p l

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

readBlobsFromDir :: MonadIO m => FilePath -> m [Blob]
readBlobsFromDir path = do
  paths <- liftIO (globDir1 (compile "[^vendor]**/*[.rb|.js|.tsx|.go|.py]") path)
  let paths' = fmap (\p -> File p (languageForFilePath p)) paths
  blobs <- traverse readBlobFromFile paths'
  pure (catMaybes blobs)

readFilePair :: forall m. (MonadFail m, MonadIO m) => File -> File -> m BlobPair
readFilePair a b = Join <$> join (maybeThese <$> readBlobFromFile a <*> readBlobFromFile b)

maybeThese :: MonadFail m => Maybe a -> Maybe b -> m (These a b)
maybeThese a b = case (a, b) of
  (Just a, Nothing) -> pure (This a)
  (Nothing, Just b) -> pure (That b)
  (Just a, Just b)  -> pure (These a b)
  _                 -> Prologue.fail "expected file pair with content on at least one side"
