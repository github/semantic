{-# LANGUAGE DeriveAnyClass, DeriveGeneric, ExplicitNamespaces, FlexibleContexts, FlexibleInstances, OverloadedStrings, RecordWildCards #-}
module Data.Blob
( File(..)
, fileForPath
, fileForTypedPath
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
, BlobPair
, maybeBlobPair
, decodeBlobPairs
, languageForBlobPair
, languageTagForBlobPair
, pathForBlobPair
, pathKeyForBlobPair
) where

import Prologue

import           Control.Effect.Error
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Edit
import           Data.JSON.Fields
import           Data.Language
import           Source.Source (Source)
import qualified Source.Source as Source
import qualified System.Path as Path
import qualified System.Path.PartClass as Path.PartClass


-- | A 'FilePath' paired with its corresponding 'Language'.
-- Unpacked to have the same size overhead as (FilePath, Language).
data File = File
  { filePath     :: FilePath
  , fileLanguage :: Language
  } deriving (Show, Eq)

-- | Prefer 'fileForTypedPath' if at all possible.
fileForPath :: FilePath  -> File
fileForPath p = File p (languageForFilePath p)

fileForTypedPath :: Path.PartClass.AbsRel ar => Path.File ar -> File
fileForTypedPath = fileForPath . Path.toString

-- | The source, path information, and language of a file read from disk.
data Blob = Blob
  { blobSource   :: Source -- ^ The UTF-8 encoded source text of the blob.
  , blobFile     :: File   -- ^ Path/language information for this blob.
  , blobOid      :: Text   -- ^ Git OID for this blob, mempty if blob is not from a git db.
  } deriving (Show, Eq)

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
nullBlob Blob{..} = Source.null blobSource

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
  deriving (Eq, Exception, Ord, Show)

noLanguageForBlob :: (Member (Error SomeException) sig, Carrier sig m) => FilePath -> m a
noLanguageForBlob blobPath = throwError (SomeException (NoLanguageForBlob blobPath))

-- | Represents a blobs suitable for diffing which can be either a blob to
-- delete, a blob to insert, or a pair of blobs to diff.
type BlobPair = Edit Blob Blob

instance FromJSON BlobPair where
  parseJSON = withObject "BlobPair" $ \o ->
    fromMaybes <$> (o .:? "before") <*> (o .:? "after")
    >>= maybeM (Prelude.fail "Expected object with 'before' and/or 'after' keys only")

maybeBlobPair :: MonadFail m => Maybe Blob -> Maybe Blob -> m BlobPair
maybeBlobPair a b = maybeM (Prologue.fail "expected file pair with content on at least one side") (fromMaybes a b)

languageForBlobPair :: BlobPair -> Language
languageForBlobPair = mergeEdit combine . bimap blobLanguage blobLanguage where
  combine a b
    | a == Unknown || b == Unknown = Unknown
    | otherwise                    = b

pathForBlobPair :: BlobPair -> FilePath
pathForBlobPair = blobPath . mergeEdit (const id)

languageTagForBlobPair :: BlobPair -> [(String, String)]
languageTagForBlobPair pair = showLanguage (languageForBlobPair pair)
  where showLanguage = pure . (,) "language" . show

pathKeyForBlobPair :: BlobPair -> FilePath
pathKeyForBlobPair = mergeEdit combine . bimap blobPath blobPath where
   combine before after | before == after = after
                        | otherwise       = before <> " -> " <> after

instance ToJSONFields Blob where
  toJSONFields p = [ "path" .= blobPath p, "language" .= blobLanguage p]

decodeBlobPairs :: BL.ByteString -> Either String [BlobPair]
decodeBlobPairs = fmap blobs <$> eitherDecode
