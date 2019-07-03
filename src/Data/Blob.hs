{-# LANGUAGE DeriveAnyClass, ExplicitNamespaces, PatternSynonyms #-}
module Data.Blob
( File(..)
, fileForPath
, Blob(..)
, Blobs(..)
, blobLanguage
, NoLanguageForBlob (..)
, blobPath
, makeBlob
, decodeBlobs
, nullBlob
, sourceBlob
, noLanguageForBlob
, type BlobPair
, pattern Diffing
, pattern Inserting
, pattern Deleting
, maybeBlobPair
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

-- | A 'FilePath' paired with its corresponding 'Language'.
-- Unpacked to have the same size overhead as (FilePath, Language).
data File = File
  { filePath     :: FilePath
  , fileLanguage :: Language
  } deriving (Show, Eq, Generic)

fileForPath :: FilePath  -> File
fileForPath p = File p (languageForFilePath p)

-- | The source, path information, and language of a file read from disk.
data Blob = Blob
  { blobSource   :: Source -- ^ The UTF-8 encoded source text of the blob.
  , blobFile     :: File   -- ^ Path/language information for this blob.
  , blobOid      :: Text   -- ^ Git OID for this blob, mempty if blob is not from a git db.
  } deriving (Show, Eq, Generic)

blobLanguage :: Blob -> Language
blobLanguage = fileLanguage . blobFile

blobPath :: Blob -> FilePath
blobPath = filePath . blobFile

makeBlob :: Source -> FilePath -> Language -> Text -> Blob
makeBlob s p l = Blob s (File p l)
{-# INLINE makeBlob #-}

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
sourceBlob filepath language source = makeBlob source filepath language mempty

inferringLanguage :: Source -> FilePath -> Language -> Blob
inferringLanguage src pth lang
  | knownLanguage lang = makeBlob src pth lang mempty
  | otherwise = makeBlob src pth (languageForFilePath pth) mempty

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
      (Just b, Just a)  -> pure $ Diffing b a
      (Just b, Nothing) -> pure $ Deleting b
      (Nothing, Just a) -> pure $ Inserting a
      _                 -> Prelude.fail "Expected object with 'before' and/or 'after' keys only"

pattern Diffing :: Blob -> Blob -> BlobPair
pattern Diffing a b = Join (These a b)

pattern Inserting :: Blob -> BlobPair
pattern Inserting a = Join (That a)

pattern Deleting :: Blob -> BlobPair
pattern Deleting b = Join (This b)

{-# COMPLETE Diffing, Inserting, Deleting #-}

maybeBlobPair :: MonadFail m => Maybe Blob -> Maybe Blob -> m BlobPair
maybeBlobPair a b = case (a, b) of
  (Just a, Nothing) -> pure (Deleting a)
  (Nothing, Just b) -> pure (Inserting b)
  (Just a, Just b)  -> pure (Diffing a b)
  _                 -> Prologue.fail "expected file pair with content on at least one side"

languageForBlobPair :: BlobPair -> Language
languageForBlobPair (Deleting b)  = blobLanguage b
languageForBlobPair (Inserting b) = blobLanguage b
languageForBlobPair (Diffing a b)
  | blobLanguage a == Unknown || blobLanguage b == Unknown
    = Unknown
  | otherwise
    = blobLanguage b

pathForBlobPair :: BlobPair -> FilePath
pathForBlobPair x = blobPath $ case x of
  (Inserting b) -> b
  (Deleting b)  -> b
  (Diffing _ b) -> b

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
  toJSONFields p = [ "path" .= blobPath p, "language" .= blobLanguage p]

decodeBlobPairs :: BL.ByteString -> Either String [BlobPair]
decodeBlobPairs = fmap blobs <$> eitherDecode
