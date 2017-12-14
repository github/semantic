module Data.Blob
( Blob(..)
, nullBlob
, sourceBlob
, BlobPair
, These(..)
, blobPairDiffing
, blobPairInserting
, blobPairDeleting
, languageForBlobPair
, languageTagForBlobPair
, pathForBlobPair
, pathKeyForBlobPair
) where

import Data.Aeson
import Data.JSON.Fields
import Data.Bifunctor.Join
import Data.Bifunctor
import Data.Language
import Data.These
import Data.Monoid
import Data.Source as Source


-- | The source, path, and language of a blob.
data Blob = Blob
  { blobSource :: Source -- ^ The UTF-8 encoded source text of the blob.
  , blobPath :: FilePath -- ^ The file path to the blob.
  , blobLanguage :: Maybe Language -- ^ The language of this blob. Nothing denotes a langauge we don't support yet.
  }
  deriving (Show, Eq)

nullBlob :: Blob -> Bool
nullBlob Blob{..} = nullSource blobSource

sourceBlob :: FilePath -> Maybe Language -> Source -> Blob
sourceBlob filepath language source = Blob source filepath language


-- | Represents a blobs suitable for diffing which can be either a blob to
-- delete, a blob to insert, or a pair of blobs to diff.
type BlobPair = Join These Blob


blobPairDiffing :: Blob -> Blob -> BlobPair
blobPairDiffing a b = Join (These a b)

blobPairInserting :: Blob -> BlobPair
blobPairInserting = Join . That

blobPairDeleting :: Blob -> BlobPair
blobPairDeleting = Join . This

languageForBlobPair :: BlobPair -> Maybe Language
languageForBlobPair (Join (This Blob{..})) = blobLanguage
languageForBlobPair (Join (That Blob{..})) = blobLanguage
languageForBlobPair (Join (These _ Blob{..})) = blobLanguage

pathForBlobPair :: BlobPair -> FilePath
pathForBlobPair (Join (This Blob{..})) = blobPath
pathForBlobPair (Join (That Blob{..})) = blobPath
pathForBlobPair (Join (These _ Blob{..})) = blobPath

languageTagForBlobPair :: BlobPair -> [(String, String)]
languageTagForBlobPair pair = maybe [] showLanguage (languageForBlobPair pair)
  where showLanguage = pure . (,) "language" . show

pathKeyForBlobPair :: BlobPair -> FilePath
pathKeyForBlobPair blobs = case bimap blobPath blobPath (runJoin blobs) of
   This before -> before
   That after -> after
   These before after | before == after -> after
                      | otherwise -> before <> " -> " <> after

instance ToJSONFields Blob where
  toJSONFields Blob{..} = [ "path" .= blobPath
                          , "filePath" .= blobPath
                          , "language" .= blobLanguage ]
