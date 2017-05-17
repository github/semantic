{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, DeriveAnyClass #-}
module Command.Files
( readFile
, readBlobPairsFromHandle
, transcode
, languageForFilePath
) where

import Prologue hiding (readFile)
import Language
import Source
import System.FilePath
import Control.Exception (catch, IOException)
import qualified Data.Text.ICU.Convert as Convert
import qualified Data.Text.ICU.Detect as Detect

import qualified Control.Applicative as A
import Data.Aeson
import qualified Data.ByteString as B
import Data.Functor.Both
import Data.String


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

-- | Read JSON encoded blobs from a handle.
readBlobPairsFromHandle :: Handle -> IO [Both SourceBlob]
readBlobPairsFromHandle h = do
  input <- B.hGetContents h
  let request = decode (toS input) :: Maybe BlobDiff
  when (isNothing request) $ die ("invalid input on " <> show h <> ", expecting JSON")
  pure $ maybe mempty sourceBlobs request


newtype BlobDiff = BlobDiff { blobs :: [BlobPair] }
  deriving (Show, Generic, FromJSON, ToJSON)

data BlobPair = BlobPair
  { path :: String
  , before :: Maybe Blob
  , after :: Maybe Blob
  } deriving (Show, Generic, FromJSON, ToJSON)

data Blob = Blob
  { content :: BlobContent
  , language :: String
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

    sourceBlob' :: FilePath -> Maybe Blob -> SourceBlob
    sourceBlob' path c = maybe (emptySourceBlob path) (sourceBlob path (language' c)) (source' c)

    language' :: Maybe Blob -> Maybe Language
    language' Nothing = Nothing
    language' (Just Blob{..}) = readMaybe language

    source' :: Maybe Blob -> Maybe Source
    source' = fmap (Source . utf8Text . content)
