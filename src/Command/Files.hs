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
import Data.These
import Data.Functor.Both
import Data.String
import Language
import Prologue hiding (readFile)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.ICU.Convert as Convert
import qualified Data.Text.ICU.Detect as Detect
import Prelude (fail)
import Source hiding (path)
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
readBlobPairsFromHandle = fmap toSourceBlobPairs . readFromHandle
  where
    toSourceBlobPairs BlobDiff{..} = toSourceBlobPair <$> blobs
    toSourceBlobPair blobs = Join (fromThese empty empty (runJoin (toSourceBlob <$> blobs)))
      where empty = emptySourceBlob (mergeThese const (runJoin (path <$> blobs)))

-- | Read JSON encoded blobs from a handle.
readBlobsFromHandle :: Handle -> IO [SourceBlob]
readBlobsFromHandle = fmap toSourceBlobs . readFromHandle
  where toSourceBlobs BlobParse{..} = fmap toSourceBlob blobs

readFromHandle :: FromJSON a => Handle -> IO a
readFromHandle h = do
  input <- BL.hGetContents h
  case decode input of
    Just d -> pure d
    Nothing -> die ("invalid input on " <> show h <> ", expecting JSON")

toSourceBlob :: Blob -> SourceBlob
toSourceBlob Blob{..} = sourceBlob path language' (Source (encodeUtf8 content))
  where language' = case language of
          "" -> languageForFilePath path
          _ -> readMaybe language


newtype BlobDiff = BlobDiff { blobs :: [BlobPair] }
  deriving (Show, Generic, FromJSON)

newtype BlobParse = BlobParse { blobs :: [Blob] }
  deriving (Show, Generic, FromJSON)

type BlobPair = Join These Blob

data Blob = Blob
  { path :: String
  , content :: Text
  , language :: String
  } deriving (Show, Generic, FromJSON)

instance FromJSON BlobPair where
  parseJSON = withObject "BlobPair" $ \o -> do
    before <- o .:? "before"
    after <- o .:? "after"
    case (before, after) of
      (Just b, Just a) -> pure $ Join (These b a)
      (Just b, Nothing) -> pure $ Join (This b)
      (Nothing, Just a) -> pure $ Join (That a)
      _ -> fail "Expected object with 'before' and/or 'after' keys only"
