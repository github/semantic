{-# LANGUAGE OverloadedStrings #-}
module Analysis.Blob
  ( Blob (..)
  , fromSource
  , blobLanguage
  , blobPath
  , nullBlob
  ) where

import           Analysis.File
import           Data.Aeson
import           Source.Language as Language
import           Source.Source as Source
import qualified System.Path as Path
import qualified System.Path.PartClass as Path.PartClass

-- | The source, path information, and language of a file read from disk.
data Blob = Blob
  { blobSource :: Source         -- ^ The UTF-8 encoded source text of the blob.
  , blobFile   :: File Language  -- ^ Path/language information for this blob.
  } deriving (Show, Eq)

instance FromJSON Blob where
  parseJSON = withObject "Blob" $ \b -> do
    src <- b .: "content"
    Right pth <- fmap Path.parse (b .: "path")
    lang <- b .: "language"
    let lang' = if knownLanguage lang then lang else Language.forPath pth
    pure (fromSource (pth :: Path.AbsRelFile) lang' src)


-- | Create a Blob from a provided path, language, and UTF-8 source.
-- The resulting Blob's span is taken from the 'totalSpan' of the source.
fromSource :: Path.PartClass.AbsRel ar => Path.File ar -> Language -> Source -> Blob
fromSource filepath language source
  = Blob source (Analysis.File.File (Path.toAbsRel filepath) (totalSpan source) language)

blobLanguage :: Blob -> Language
blobLanguage = Analysis.File.fileBody . blobFile

blobPath :: Blob -> FilePath
blobPath = Path.toString . Analysis.File.filePath . blobFile

nullBlob :: Blob -> Bool
nullBlob = Source.null . blobSource
