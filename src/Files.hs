{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, DeriveAnyClass, DuplicateRecordFields, ScopedTypeVariables #-}
module Files
( readFile
, readBlobPairsFromHandle
, readBlobsFromHandle
, languageForFilePath
) where

import Control.Exception (catch, IOException)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Blob as Blob
import Data.Functor.Both
import Data.Maybe
import Data.Semigroup
import Data.Source
import Data.String
import Data.Text
import Data.These
import GHC.Generics
import Language
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Prelude hiding (readFile)
import System.Exit
import System.FilePath
import System.IO (Handle)
import Text.Read

-- | Read a utf8-encoded file to a 'Blob'.
readFile :: forall m. MonadIO m => FilePath -> Maybe Language -> m Blob.Blob
readFile path@"/dev/null" _ = pure (Blob.emptyBlob path)
readFile path language = do
  raw <- liftIO $ (Just <$> B.readFile path) `catch` (const (pure Nothing) :: IOException -> IO (Maybe B.ByteString))
  pure $ fromMaybe (Blob.emptyBlob path) (Blob.sourceBlob path language . fromBytes <$> raw)

-- | Return a language based on a FilePath's extension, or Nothing if extension is not found or not supported.
languageForFilePath :: FilePath -> Maybe Language
languageForFilePath = languageForType . takeExtension

-- | Read JSON encoded blob pairs from a handle.
readBlobPairsFromHandle :: MonadIO m => Handle -> m [Both Blob.Blob]
readBlobPairsFromHandle = fmap toBlobPairs . readFromHandle
  where
    toBlobPairs BlobDiff{..} = toBlobPair <$> blobs
    toBlobPair blobs = Join (fromThese empty empty (runJoin (toBlob <$> blobs)))
      where empty = Blob.emptyBlob (mergeThese const (runJoin (path <$> blobs)))

-- | Read JSON encoded blobs from a handle.
readBlobsFromHandle :: MonadIO m => Handle -> m [Blob.Blob]
readBlobsFromHandle = fmap toBlobs . readFromHandle
  where toBlobs BlobParse{..} = fmap toBlob blobs

readFromHandle :: (FromJSON a, MonadIO m) => Handle -> m a
readFromHandle h = do
  input <- liftIO $ BL.hGetContents h
  case decode input of
    Just d -> pure d
    Nothing -> liftIO $ die ("invalid input on " <> show h <> ", expecting JSON")

toBlob :: Blob -> Blob.Blob
toBlob Blob{..} = Blob.sourceBlob path language' (fromText content)
  where language' = case language of
          "" -> languageForFilePath path
          _ -> readMaybe language


newtype BlobDiff = BlobDiff { blobs :: [BlobPair] }
  deriving (Show, Generic, FromJSON)

newtype BlobParse = BlobParse { blobs :: [Blob] }
  deriving (Show, Generic, FromJSON)

type BlobPair = Join These Blob

data Blob = Blob
  { path :: FilePath
  , content :: Text
  , language :: String
  }
  deriving (Show, Generic, FromJSON)

instance FromJSON BlobPair where
  parseJSON = withObject "BlobPair" $ \o -> do
    before <- o .:? "before"
    after <- o .:? "after"
    case (before, after) of
      (Just b, Just a) -> pure $ Join (These b a)
      (Just b, Nothing) -> pure $ Join (This b)
      (Nothing, Just a) -> pure $ Join (That a)
      _ -> fail "Expected object with 'before' and/or 'after' keys only"
