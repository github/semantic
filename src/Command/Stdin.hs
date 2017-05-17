{-# LANGUAGE RecordWildCards, DeriveGeneric, DeriveAnyClass, TypeSynonymInstances #-}
module Command.Stdin
( readStdin
) where

import Command.Files (languageForFilePath)
import qualified Control.Applicative as A
import Data.Aeson
import qualified Data.ByteString as B
import Data.Functor.Both
import Data.String
import Prologue
import Source

readStdin :: IO [Both SourceBlob]
readStdin = do
  input <- B.getContents
  let request = decode (toS input) :: Maybe BlobDiff
  pure $ maybe mempty sourceBlobs request


newtype BlobDiff = BlobDiff { blobs :: [BlobPair] }
  deriving (Show, Generic, FromJSON, ToJSON)

data BlobPair = BlobPair
  { path :: String
  , before :: Maybe BlobContent
  , after :: Maybe BlobContent
  } deriving (Show, Generic, FromJSON, ToJSON)

newtype BlobContent = BlobContent { utf8Text :: ByteString }
  deriving (Eq, Show)

instance ToJSON BlobContent where
  toJSON = String . decodeUtf8 . utf8Text

instance FromJSON BlobContent where
  parseJSON (String t) = (pure . BlobContent . encodeUtf8) t
  parseJSON _ = A.empty

sourceBlobs :: BlobDiff -> [Both SourceBlob]
sourceBlobs BlobDiff{..} = toSourceBlob <$> blobs
  where
    toSourceBlob :: BlobPair -> Both SourceBlob
    toSourceBlob BlobPair{..} = fmap (sourceBlob' path) (both before after)

    sourceBlob' :: FilePath -> Maybe BlobContent -> SourceBlob
    sourceBlob' path maybeContent = maybe (emptySourceBlob path) (sourceBlob path (languageForFilePath path)) (source' maybeContent)

    source' :: Maybe BlobContent -> Maybe Source
    source' = fmap (Source . utf8Text)
