{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric, DeriveAnyClass, TypeSynonymInstances #-}
module Command.Files
( readFile
, transcode
, languageForFilePath
, readStdin
) where

import Prologue hiding (readFile)
import Language
import Source
import qualified Data.ByteString as B
import System.FilePath
import Control.Exception (catch, IOException)
import qualified Data.Text.ICU.Convert as Convert
import qualified Data.Text.ICU.Detect as Detect

import qualified Control.Applicative as A
import Data.String
import Data.Aeson
import Data.Functor.Both

-- | Read a file to a SourceBlob, transcoding to UTF-8 along the way.
readFile :: FilePath -> Maybe Language -> IO SourceBlob
readFile path language = do
  raw <- (Just <$> B.readFile path) `catch` (const (pure Nothing) :: IOException -> IO (Maybe ByteString))
  source <- traverse transcode raw
  pure $ fromMaybe (emptySourceBlob path) (sourceBlob path language <$> source)

-- | Transcode a ByteString to a unicode Source.
transcode :: B.ByteString -> IO Source
transcode text = fromText <$> do
  match <- Detect.detectCharset text
  converter <- Convert.open match Nothing
  pure $ Convert.toUnicode converter text

-- | Return a language based on a FilePath's extension, or Nothing if extension is not found or not supported.
languageForFilePath :: FilePath -> Maybe Language
languageForFilePath = languageForType . toS . takeExtension


-- PROTOTYPE

readStdin :: IO [Both SourceBlob]
readStdin = do
  input <- B.getContents
  let request = decode (toS input) :: Maybe DiffRequest
  pure $ maybe mempty sourceBlobs request


newtype DiffRequest = DiffRequest
  { blobs :: [BlobPair]
  } deriving (Show, Generic, FromJSON, ToJSON)

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

sourceBlobs :: DiffRequest -> [Both SourceBlob]
sourceBlobs DiffRequest{..} = toSourceBlob <$> blobs
  where
    toSourceBlob :: BlobPair -> Both SourceBlob
    toSourceBlob BlobPair{..} = fmap (sourceBlob' path) (both before after)

    sourceBlob' :: FilePath -> Maybe BlobContent -> SourceBlob
    sourceBlob' path maybeContent = maybe (emptySourceBlob path) (sourceBlob path (languageForFilePath path)) (source' maybeContent)

    source' :: Maybe BlobContent -> Maybe Source
    source' = fmap (Source . utf8Text)
