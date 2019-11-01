{-# LANGUAGE DataKinds, DeriveAnyClass, GADTs, StandaloneDeriving #-}

module Data.Handle
  ( Handle (..)
  , getHandle
  , stdin
  , stdout
  , stderr
  , readBlobsFromHandle
  , readPathsFromHandle
  , readBlobPairsFromHandle
  , readFromHandle
  , openFileForReading
  , InvalidJSONException (..)
  ) where

import Prologue

import           Control.Exception (throw)
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified System.IO as IO

import Data.Blob

data Handle mode where
  ReadHandle  :: IO.Handle -> Handle 'IO.ReadMode
  WriteHandle :: IO.Handle -> Handle 'IO.WriteMode

deriving instance Eq   (Handle mode)
deriving instance Show (Handle mode)

getHandle :: Handle mode -> IO.Handle
getHandle (ReadHandle  handle) = handle
getHandle (WriteHandle handle) = handle

stdin :: Handle 'IO.ReadMode
stdin = ReadHandle IO.stdin

stdout :: Handle 'IO.WriteMode
stdout = WriteHandle IO.stdout

stderr :: Handle 'IO.WriteMode
stderr = WriteHandle IO.stderr

openFileForReading :: FilePath -> IO (Handle 'IO.ReadMode)
openFileForReading path = ReadHandle <$> IO.openFile path IO.ReadMode

-- | Read JSON encoded blobs from a handle.
readBlobsFromHandle :: MonadIO m => Handle 'IO.ReadMode -> m [Blob]
readBlobsFromHandle = fmap blobs <$> readFromHandle

-- | Read line delimited paths from a handle
readPathsFromHandle :: MonadIO m => Handle 'IO.ReadMode -> m [FilePath]
readPathsFromHandle (ReadHandle h) = liftIO $ fmap BLC.unpack . BLC.lines <$> BL.hGetContents h

-- | Read JSON encoded blob pairs from a handle.
readBlobPairsFromHandle :: MonadIO m => Handle 'IO.ReadMode -> m [BlobPair]
readBlobPairsFromHandle = fmap blobs <$> readFromHandle

newtype InvalidJSONException = InvalidJSONException String
  deriving (Eq, Show, Exception)

-- | Read JSON-encoded data from a 'Handle'. Throws
-- 'InvalidJSONException' on parse failure.
readFromHandle :: (FromJSON a, MonadIO m) => Handle 'IO.ReadMode -> m a
readFromHandle (ReadHandle h) = do
  input <- liftIO $ BL.hGetContents h
  case eitherDecode input of
    Left e  -> throw (InvalidJSONException e)
    Right d -> pure d
