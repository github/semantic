{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, DeriveAnyClass, DuplicateRecordFields #-}
module Command.Files
( readFile
, readBlobPairsFromHandle
, readBlobsFromHandle
, transcode
, languageForFilePath
) where

import Control.Exception (catch, IOException)
import Data.Aeson
import Data.Functor.Both
import Data.String
import Language
import Prologue hiding (readFile)
import qualified Control.Applicative as A
import qualified Data.ByteString as B
import qualified Data.Text.ICU.Convert as Convert
import qualified Data.Text.ICU.Detect as Detect
import Source
import System.FilePath


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

-- | Read JSON encoded blob pairs from a handle.
readBlobPairsFromHandle :: Handle -> IO [Both SourceBlob]
readBlobPairsFromHandle = readFromHandle toSourceBlobPairs
  where
    toSourceBlobPairs BlobDiff{..} = toSourceBlobPair <$> blobs
    toSourceBlobPair BlobPair{..} = fmap (maybe (emptySourceBlob path) toSourceBlob) (both before after)

-- | Read JSON encoded blobs from a handle.
readBlobsFromHandle :: Handle -> IO [SourceBlob]
readBlobsFromHandle = readFromHandle toSourceBlobs
  where
    toSourceBlobs BlobParse{..} = fmap toSourceBlob blobs

readFromHandle :: (FromJSON a, Monoid b) => (a -> b) -> Handle -> IO b
readFromHandle f h = do
  input <- B.hGetContents h
  let d = decode (toS input)
  when (isNothing d) $ die ("invalid input on " <> show h <> ", expecting JSON")
  pure $ maybe mempty f d

toSourceBlob :: Blob -> SourceBlob
toSourceBlob Blob{..} = sourceBlob path (readMaybe language) (Source (utf8Text content))


newtype BlobDiff = BlobDiff { blobs :: [BlobPair] }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype BlobParse = BlobParse { blobs :: [Blob] }
  deriving (Show, Generic, FromJSON, ToJSON)

data BlobPair = BlobPair
  { path :: String
  , before :: Maybe Blob
  , after :: Maybe Blob
  } deriving (Show, Generic, FromJSON, ToJSON)

data Blob = Blob
  { path :: String
  , content :: BlobContent
  , language :: String
  } deriving (Show, Generic, FromJSON, ToJSON)

newtype BlobContent = BlobContent { utf8Text :: ByteString }
  deriving (Eq, Show)

instance ToJSON BlobContent where
  toJSON = String . decodeUtf8 . utf8Text

instance FromJSON BlobContent where
  parseJSON (String t) = (pure . BlobContent . encodeUtf8) t
  parseJSON _ = A.empty
