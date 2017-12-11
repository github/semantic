module Data.Blob
( Blob(..)
, BlobKind(..)
, These(..)
, modeToDigits
, nullBlob
, sourceBlob
, nullOid
, BlobPair
, blobPairDiffing
, blobPairInserting
, blobPairDeleting
, languageForBlobPair
, languageTagForBlobPair
, pathForBlobPair
) where

import Data.ByteString.Char8 (ByteString, pack)
import Data.Bifunctor.Join
import Data.Language
import Data.These
import Data.Source as Source
import Data.Word
import Numeric


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


-- | The source, oid, path, kind and language of a blob.
data Blob = Blob
  { blobSource :: Source -- ^ The UTF-8 encoded source text of the blob.
  , blobOid :: ByteString -- ^ The Git object ID (SHA-1) of the blob.
  , blobPath :: FilePath -- ^ The file path to the blob.
  , blobLanguage :: Maybe Language -- ^ The language of this blob. Nothing denotes a langauge we don't support yet.
  }
  deriving (Show, Eq)

-- | The kind and file mode of a 'Blob'.
data BlobKind = PlainBlob Word32 | ExecutableBlob Word32 | SymlinkBlob Word32
  deriving (Show, Eq)

modeToDigits :: BlobKind -> ByteString
modeToDigits (PlainBlob mode) = pack $ showOct mode ""
modeToDigits (ExecutableBlob mode) = pack $ showOct mode ""
modeToDigits (SymlinkBlob mode) = pack $ showOct mode ""

nullBlob :: Blob -> Bool
nullBlob Blob{..} = blobOid == nullOid || nullSource blobSource

sourceBlob :: FilePath -> Maybe Language -> Source -> Blob
sourceBlob filepath language source = Blob source nullOid filepath language

nullOid :: ByteString
nullOid = "0000000000000000000000000000000000000000"
