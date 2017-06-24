module Data.Blob where

import Data.Source as Source
import Language
import Numeric
import Prologue

-- | The source, oid, path, and Maybe BlobKind of a blob.
data Blob = Blob
  { source :: Source -- ^ The UTF-8 encoded source text of the blob.
  , oid :: ByteString -- ^ The Git object ID (SHA-1) of the blob.
  , path :: FilePath -- ^ The file path to the blob.
  , blobKind :: Maybe BlobKind -- ^ The kind of blob, Nothing denotes a blob that doesn't exist (e.g. on one side of a diff for adding a new file or deleting a file).
  , blobLanguage :: Maybe Language -- ^ The language of this blob. Nothing denotes a langauge we don't support yet.
  }
  deriving (Show, Eq)

-- | The kind and file mode of a 'Blob'.
data BlobKind = PlainBlob Word32  | ExecutableBlob Word32 | SymlinkBlob Word32
  deriving (Show, Eq)

modeToDigits :: BlobKind -> ByteString
modeToDigits (PlainBlob mode) = toS $ showOct mode ""
modeToDigits (ExecutableBlob mode) = toS $ showOct mode ""
modeToDigits (SymlinkBlob mode) = toS $ showOct mode ""

-- | The default plain blob mode
defaultPlainBlob :: BlobKind
defaultPlainBlob = PlainBlob 0o100644

emptyBlob :: FilePath -> Blob
emptyBlob filepath = Blob mempty nullOid filepath Nothing Nothing

nullBlob :: Blob -> Bool
nullBlob Blob{..} = oid == nullOid || Source.null source

blobExists :: Blob -> Bool
blobExists Blob{..} = isJust blobKind

sourceBlob :: FilePath -> Maybe Language -> Source -> Blob
sourceBlob filepath language source = Blob source nullOid filepath (Just defaultPlainBlob) language

-- | Map blobs with Nothing blobKind to empty blobs.
idOrEmptyBlob :: Blob -> Blob
idOrEmptyBlob blob = if isNothing (blobKind blob)
                           then blob { oid = nullOid, blobKind = Nothing }
                           else blob

nullOid :: ByteString
nullOid = "0000000000000000000000000000000000000000"
