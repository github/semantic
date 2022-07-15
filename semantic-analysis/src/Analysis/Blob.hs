{-# LANGUAGE OverloadedStrings #-}
module Analysis.Blob
  ( Blob (..)
  , fromSource
  , blobLanguage
  , blobPath
  , blobFilePath
  , nullBlob
  ) where

import Analysis.File as A
import Analysis.Reference as A
import Data.Aeson
import Source.Language as Language
import Source.Source as Source

-- | The source, path information, and language of a file read from disk.
data Blob = Blob
  { blobSource :: Source         -- ^ The UTF-8 encoded source text of the blob.
  , blobFile   :: File Language  -- ^ Path/language information for this blob.
  } deriving (Show, Eq)

instance FromJSON Blob where
  parseJSON = withObject "Blob" $ \b -> do
    src <- b .: "content"
    pth <- b .: "path"
    lang <- b .: "language"
    let lang' = if knownLanguage lang then lang else Language.forPath pth
    pure (fromSource pth lang' src)


-- | Create a Blob from a provided path, language, and UTF-8 source.
-- The resulting Blob's span is taken from the 'totalSpan' of the source.
fromSource :: FilePath -> Language -> Source -> Blob
fromSource filepath language source
  = Blob source (A.File (A.Reference filepath (totalSpan source)) language)

blobLanguage :: Blob -> Language
blobLanguage = A.fileBody . blobFile

blobPath :: Blob -> FilePath
blobPath = A.refPath . A.fileRef . blobFile

-- | Show FilePath for error or json outputs.
blobFilePath :: Blob -> String
blobFilePath = blobPath

nullBlob :: Blob -> Bool
nullBlob = Source.null . blobSource
