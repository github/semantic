{-# LANGUAGE DeriveGeneric #-}
module Data.Blob
( File(..)
, Blob(..)
, blobLanguage
, blobPath
, makeBlob
) where

import Data.Language
import Data.Source as Source
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A 'FilePath' paired with its corresponding 'Language'.
-- Unpacked to have the same size overhead as (FilePath, Language).
data File = File
  { filePath     :: FilePath
  , fileLanguage :: Language
  }
  deriving (Show, Eq, Generic)

-- | The source, path information, and language of a file read from disk.
data Blob = Blob
  { blobSource   :: Source -- ^ The UTF-8 encoded source text of the blob.
  , blobFile     :: File   -- ^ Path/language information for this blob.
  , blobOid      :: Text   -- ^ Git OID for this blob, mempty if blob is not from a git db.
  }
  deriving (Show, Eq, Generic)

blobLanguage :: Blob -> Language
blobLanguage = fileLanguage . blobFile

blobPath :: Blob -> FilePath
blobPath = filePath . blobFile

makeBlob :: Source -> FilePath -> Language -> Text -> Blob
makeBlob s p l = Blob s (File p l)
{-# INLINE makeBlob #-}
