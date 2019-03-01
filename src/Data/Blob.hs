{-# LANGUAGE DeriveAnyClass, ExplicitNamespaces, PatternSynonyms #-}
module Data.Blob
( Blob(..)
, Blobs(..)
, decodeBlobs
, nullBlob
, sourceBlob
, noLanguageForBlob
, type BlobPair
, pattern Diffing
, pattern Inserting
, pattern Deleting
, decodeBlobPairs
, languageForBlobPair
, languageTagForBlobPair
, pathForBlobPair
, pathKeyForBlobPair
) where

import Prologue

import           Control.Effect
import           Control.Effect.Error
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.JSON.Fields
import           Data.Language
import           Data.Source as Source

-- | The source, path, and language of a blob.
data Blob = Blob
  { blobSource   :: Source   -- ^ The UTF-8 encoded source text of the blob.
  , blobPath     :: FilePath -- ^ The file path to the blob.
  , blobLanguage :: Language -- ^ The language of this blob.
  }
  deriving (Show, Eq, Generic)

newtype Blobs a = Blobs { blobs :: [a] }
  deriving (Generic, FromJSON)

instance FromJSON Blob where
  parseJSON = withObject "Blob" $ \b -> inferringLanguage
    <$> b .: "content"
    <*> b .: "path"
    <*> b .: "language"

nullBlob :: Blob -> Bool
nullBlob Blob{..} = nullSource blobSource

sourceBlob :: FilePath -> Language -> Source -> Blob
sourceBlob filepath language source = Blob source filepath language

inferringLanguage :: Source -> FilePath -> Language -> Blob
inferringLanguage src pth lang
  | knownLanguage lang = Blob src pth lang
  | otherwise = Blob src pth (languageForFilePath pth)

decodeBlobs :: BL.ByteString -> Either String [Blob]
decodeBlobs = fmap blobs <$> eitherDecode

-- | An exception indicating that we’ve tried to diff or parse a blob of unknown language.
newtype NoLanguageForBlob = NoLanguageForBlob FilePath
  deriving (Eq, Exception, Ord, Show, Typeable)

noLanguageForBlob :: (Member (Error SomeException) sig, Carrier sig m) => FilePath -> m a
noLanguageForBlob blobPath = throwError (SomeException (NoLanguageForBlob blobPath))

-- | Represents a blobs suitable for diffing which can be either a blob to
-- delete, a blob to insert, or a pair of blobs to diff.
type BlobPair = Join These Blob

instance FromJSON BlobPair where
  parseJSON = withObject "BlobPair" $ \o -> do
    before <- o .:? "before"
    after <- o .:? "after"
    case (before, after) of
      (Just b, Just a)  -> pure $ Join (These b a)
      (Just b, Nothing) -> pure $ Join (This b)
      (Nothing, Just a) -> pure $ Join (That a)
      _                 -> Prelude.fail "Expected object with 'before' and/or 'after' keys only"

pattern Diffing :: Blob -> Blob -> BlobPair
pattern Diffing a b = Join (These a b)

pattern Inserting :: Blob -> BlobPair
pattern Inserting a = Join (That a)

pattern Deleting :: Blob -> BlobPair
pattern Deleting b = Join (This b)

{-# COMPLETE Diffing, Inserting, Deleting #-}

languageForBlobPair :: BlobPair -> Language
languageForBlobPair (Deleting Blob{..})  = blobLanguage
languageForBlobPair (Inserting Blob{..}) = blobLanguage
languageForBlobPair (Diffing a b)
  | blobLanguage a == Unknown || blobLanguage b == Unknown
    = Unknown
  | otherwise
    = blobLanguage b

pathForBlobPair :: BlobPair -> FilePath
pathForBlobPair (Deleting Blob{..})  = blobPath
pathForBlobPair (Inserting Blob{..}) = blobPath
pathForBlobPair (Diffing _ Blob{..}) = blobPath

languageTagForBlobPair :: BlobPair -> [(String, String)]
languageTagForBlobPair pair = showLanguage (languageForBlobPair pair)
  where showLanguage = pure . (,) "language" . show

pathKeyForBlobPair :: BlobPair -> FilePath
pathKeyForBlobPair blobs = case bimap blobPath blobPath (runJoin blobs) of
   This before -> before
   That after -> after
   These before after | before == after -> after
                      | otherwise -> before <> " -> " <> after

instance ToJSONFields Blob where
  toJSONFields Blob{..} = [ "path" .= blobPath, "language" .= blobLanguage ]

decodeBlobPairs :: BL.ByteString -> Either String [BlobPair]
decodeBlobPairs = fmap blobs <$> eitherDecode
