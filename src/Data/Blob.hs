{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Blob
( Blob(..)
, Blobs(..)
, blobLanguage
, NoLanguageForBlob (..)
, blobPath
, decodeBlobs
, nullBlob
, fromSource
, moduleForBlob
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

import           Analysis.File (fileBody)
import qualified Analysis.File
import           Analysis.Language as Language
import           Control.Effect.Error
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Edit
import           Data.JSON.Fields
import           Data.Module
import           Source.Source (Source, totalSpan)
import qualified Source.Source as Source
import qualified System.FilePath as FP
import qualified System.Path as Path
import qualified System.Path.PartClass as Path.PartClass

type File = Analysis.File.File Language

-- | The source, path information, and language of a file read from disk.
data Blob = Blob
  { blobSource :: Source -- ^ The UTF-8 encoded source text of the blob.
  , blobFile   :: File   -- ^ Path/language information for this blob.
  } deriving (Show, Eq)

blobLanguage :: Blob -> Language
blobLanguage = Analysis.File.fileBody . blobFile

blobPath :: Blob -> FilePath
blobPath = Path.toString . Analysis.File.filePath . blobFile

newtype Blobs a = Blobs { blobs :: [a] }
  deriving (Generic, FromJSON)

instance FromJSON Blob where
  parseJSON = withObject "Blob" $ \b -> do
    src <- b .: "content"
    Right pth <- fmap Path.parse (b .: "path")
    lang <- b .: "language"
    let lang' = if knownLanguage lang then lang else Language.forPath pth
    pure (Blob src (Analysis.File.File pth (totalSpan src) lang'))

nullBlob :: Blob -> Bool
nullBlob Blob{..} = Source.null blobSource

-- | Create a Blob from a provided path, language, and UTF-8 source.
-- The resulting Blob's span is taken from the 'totalSpan' of the source.
fromSource :: Path.PartClass.AbsRel ar => Path.File ar -> Language -> Source -> Blob
fromSource filepath language source
  = Blob source (Analysis.File.File (Path.toAbsRel filepath) (totalSpan source) language)

decodeBlobs :: BL.ByteString -> Either String [Blob]
decodeBlobs = fmap blobs <$> eitherDecode

-- | An exception indicating that we’ve tried to diff or parse a blob of unknown language.
newtype NoLanguageForBlob = NoLanguageForBlob FilePath
  deriving (Eq, Exception, Ord, Show)

noLanguageForBlob :: Has (Error SomeException) sig m => FilePath -> m a
noLanguageForBlob blobPath = throwError (SomeException (NoLanguageForBlob blobPath))

-- | Construct a 'Module' for a 'Blob' and @term@, relative to some root 'FilePath'.
moduleForBlob :: Maybe FilePath -- ^ The root directory relative to which the module will be resolved, if any. TODO: typed paths
              -> Blob             -- ^ The 'Blob' containing the module.
              -> term             -- ^ The @term@ representing the body of the module.
              -> Module term    -- ^ A 'Module' named appropriate for the 'Blob', holding the @term@, and constructed relative to the root 'FilePath', if any.
moduleForBlob rootDir b = Module info
  where root = fromMaybe (FP.takeDirectory (blobPath b)) rootDir
        info = ModuleInfo (FP.makeRelative root (blobPath b)) (languageToText (blobLanguage b)) mempty

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
