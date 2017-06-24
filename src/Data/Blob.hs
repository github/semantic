module Data.Blob where

import Data.Source as Source
import Language
import Numeric
import Prologue

-- | The source, oid, path, and Maybe SourceKind of a blob.
data SourceBlob = SourceBlob
  { source :: Source -- ^ The UTF-8 encoded source text of the blob.
  , oid :: ByteString -- ^ The Git object ID (SHA-1) of the blob.
  , path :: FilePath -- ^ The file path to the blob.
  , blobKind :: Maybe SourceKind -- ^ The kind of blob, Nothing denotes a blob that doesn't exist (e.g. on one side of a diff for adding a new file or deleting a file).
  , blobLanguage :: Maybe Language -- ^ The language of this blob. Nothing denotes a langauge we don't support yet.
  }
  deriving (Show, Eq)

-- | The kind of a blob, along with it's file mode.
data SourceKind = PlainBlob Word32  | ExecutableBlob Word32 | SymlinkBlob Word32
  deriving (Show, Eq)

modeToDigits :: SourceKind -> ByteString
modeToDigits (PlainBlob mode) = toS $ showOct mode ""
modeToDigits (ExecutableBlob mode) = toS $ showOct mode ""
modeToDigits (SymlinkBlob mode) = toS $ showOct mode ""

-- | The default plain blob mode
defaultPlainBlob :: SourceKind
defaultPlainBlob = PlainBlob 0o100644

emptySourceBlob :: FilePath -> SourceBlob
emptySourceBlob filepath = SourceBlob mempty nullOid filepath Nothing Nothing

nullBlob :: SourceBlob -> Bool
nullBlob SourceBlob{..} = oid == nullOid || Source.null source

blobExists :: SourceBlob -> Bool
blobExists SourceBlob{..} = isJust blobKind

sourceBlob :: FilePath -> Maybe Language -> Source -> SourceBlob
sourceBlob filepath language source = SourceBlob source nullOid filepath (Just defaultPlainBlob) language

-- | Map blobs with Nothing blobKind to empty blobs.
idOrEmptySourceBlob :: SourceBlob -> SourceBlob
idOrEmptySourceBlob blob = if isNothing (blobKind blob)
                           then blob { oid = nullOid, blobKind = Nothing }
                           else blob

nullOid :: ByteString
nullOid = "0000000000000000000000000000000000000000"
