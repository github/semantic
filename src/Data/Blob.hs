{-# LANGUAGE DeriveAnyClass #-}
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

import Prologue
import Proto3.Suite
import Data.Aeson
import Data.JSON.Fields
import Data.Language
import Data.Source as Source
import qualified Proto3.Wire.Encode as Encode
import qualified Proto3.Wire.Decode as Decode

-- | The source, path, and language of a blob.
data Blob = Blob
  { blobSource :: Source -- ^ The UTF-8 encoded source text of the blob.
  , blobPath :: FilePath -- ^ The file path to the blob.
  , blobLanguage :: Language -- ^ The language of this blob.
  }
  deriving (Show, Eq, Generic, Message, Named)

instance FromJSON Blob where
  parseJSON = withObject "Blob" $ \b -> inferringLanguage
    <$> b .: "content"
    <*> b .: "path"
    <*> b .: "language"

nullBlob :: Blob -> Bool
nullBlob Blob{..} = nullSource blobSource

sourceBlob :: FilePath -> Language -> Source -> Blob
sourceBlob filepath language source = Blob source filepath language

inferringLanguage :: Source -> FilePath -> Language -> Blob
inferringLanguage src pth lang
  | knownLanguage lang = Blob src pth lang
  | otherwise = Blob src pth (languageForFilePath pth)

-- | Represents a blobs suitable for diffing which can be either a blob to
-- delete, a blob to insert, or a pair of blobs to diff.
type BlobPair = Join These Blob

instance Message BlobPair where
  encodeMessage _ pair = case pair of
    (Join (These a b)) -> Encode.embedded 1 (encodeMessage 1 a) <> Encode.embedded 2 (encodeMessage 1 b)
    (Join (This a)) -> Encode.embedded 1 (encodeMessage 1 a)
    (Join (That b)) -> Encode.embedded 2 (encodeMessage 1 b)
  decodeMessage _ = Join <$> (these <|> this <|> that)
    where
      embeddedAt parser = Decode.at (Decode.embedded'' parser)
      these = These <$> embeddedAt (decodeMessage 1) 1 <*> embeddedAt (decodeMessage 1) 2
      this = This <$> embeddedAt (decodeMessage 1) 1
      that = That <$> embeddedAt (decodeMessage 1) 2
  dotProto _ =
    [ DotProtoMessageField $ DotProtoField 1 (Prim . Named $ Single "Blob") (Single "before") [] Nothing
    , DotProtoMessageField $ DotProtoField 2 (Prim . Named $ Single "Blob") (Single "after") [] Nothing
    ]

instance Named BlobPair where
  nameOf _ = "BlobPair"

instance FromJSON BlobPair where
  parseJSON = withObject "BlobPair" $ \o -> do
    before <- o .:? "before"
    after <- o .:? "after"
    case (before, after) of
      (Just b, Just a)  -> pure $ Join (These b a)
      (Just b, Nothing) -> pure $ Join (This b)
      (Nothing, Just a) -> pure $ Join (That a)
      _                 -> Prelude.fail "Expected object with 'before' and/or 'after' keys only"

blobPairDiffing :: Blob -> Blob -> BlobPair
blobPairDiffing a b = Join (These a b)

blobPairInserting :: Blob -> BlobPair
blobPairInserting = Join . That

blobPairDeleting :: Blob -> BlobPair
blobPairDeleting = Join . This

languageForBlobPair :: BlobPair -> Language
languageForBlobPair (Join (This Blob{..})) = blobLanguage
languageForBlobPair (Join (That Blob{..})) = blobLanguage
languageForBlobPair (Join (These a b))
  | blobLanguage a == Unknown || blobLanguage b == Unknown
    = Unknown
  | otherwise
    = blobLanguage b

pathForBlobPair :: BlobPair -> FilePath
pathForBlobPair (Join (This Blob{..})) = blobPath
pathForBlobPair (Join (That Blob{..})) = blobPath
pathForBlobPair (Join (These _ Blob{..})) = blobPath

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
  toJSONFields Blob{..} = [ "path" .= blobPath, "language" .= blobLanguage ]
