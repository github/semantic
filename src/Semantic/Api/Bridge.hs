{-# LANGUAGE FunctionalDependencies, LambdaCase #-}
module Semantic.Api.Bridge
  ( APIBridge (..)
  , APIConvert (..)
  , (#?)
  ) where

import           Control.Lens
import qualified Data.Blob as Data
import qualified Data.Language as Data
import qualified Data.Text as T
import qualified Semantic.Api.LegacyTypes as Legacy
import qualified Semantic.Proto.SemanticPB as API
import           Source.Source (fromText, toText)
import qualified Source.Span as Source

-- | An @APIBridge x y@ instance describes an isomorphism between @x@ and @y@.
-- This is suitable for types such as 'Pos' which are representationally equivalent
-- in their API, legacy, and native forms. All 'Lens' laws apply.
--
-- Foreign to native: @x^.bridging@
-- Native to foreign: @bridging # x@
-- Native to 'Just' foreign: @bridging #? x@.
-- 'Maybe' foreign to 'Maybe' native: @x >>= preview bridging@
class APIBridge api native | api -> native where
  bridging :: Iso' api native

-- | An @APIConvert x y@ instance describes a partial isomorphism between @x@ and @y@.
-- This is suitable for types containing nested records therein, such as 'Span'.
-- (The isomorphism must be partial, given that a protobuf record can have Nothing
-- for all its fields, which means we cannot convert to a native format.)
--
-- Foreign to native: this is a type error, unless the native is a Monoid
-- Foreign to 'Maybe' native: @x^?converting@
-- Native to foreign: @converting # x@
-- Native to 'Just' foreign: @converting #? x@
class APIConvert api native | api -> native where
  converting :: Prism' api native

-- | A helper function for turning 'bridging' around and
-- extracting 'Just' values from it.
(#?) :: AReview t s -> s -> Maybe t
rev #? item = item ^? re rev
infixr 8 #?

instance APIBridge Legacy.Position Source.Pos where
  bridging = iso fromAPI toAPI where
    toAPI Source.Pos{..}        = Legacy.Position line column
    fromAPI Legacy.Position{..} = Source.Pos line column

instance APIBridge API.Position Source.Pos where
  bridging = iso fromAPI toAPI where
    toAPI Source.Pos{..}     = API.Position (fromIntegral line) (fromIntegral column)
    fromAPI API.Position{..} = Source.Pos (fromIntegral line) (fromIntegral column)

instance APIConvert API.Span Source.Span where
  converting = prism' toAPI fromAPI where
    toAPI Source.Span{..} = API.Span (bridging #? spanStart) (bridging #? spanEnd)
    fromAPI API.Span{..} = Source.Span <$> (start >>= preview bridging) <*> (end >>= preview bridging)

instance APIConvert Legacy.Span Source.Span where
  converting = prism' toAPI fromAPI where
    toAPI Source.Span{..} = Legacy.Span (bridging #? spanStart) (bridging #? spanEnd)
    fromAPI Legacy.Span {..} = Source.Span <$> (start >>= preview bridging) <*> (end >>= preview bridging)

instance APIBridge T.Text Data.Language where
  bridging = iso Data.textToLanguage Data.languageToText

instance APIBridge API.Blob Data.Blob where
  bridging = iso apiBlobToBlob blobToApiBlob where
    blobToApiBlob b = API.Blob (toText (Data.blobSource b)) (T.pack (Data.blobPath b)) (bridging # Data.blobLanguage b)
    apiBlobToBlob API.Blob{..} = Data.makeBlob (fromText content) (T.unpack path) (language ^. bridging) mempty


instance APIConvert API.BlobPair Data.BlobPair where
  converting = prism' blobPairToApiBlobPair apiBlobPairToBlobPair where

    apiBlobPairToBlobPair (API.BlobPair (Just before) (Just after)) = Just $ Data.Diffing (before^.bridging) (after^.bridging)
    apiBlobPairToBlobPair (API.BlobPair (Just before) Nothing) = Just $ Data.Deleting (before^.bridging)
    apiBlobPairToBlobPair (API.BlobPair Nothing (Just after)) = Just $ Data.Inserting (after^.bridging)
    apiBlobPairToBlobPair _ = Nothing

    blobPairToApiBlobPair (Data.Diffing before after) = API.BlobPair (bridging #? before) (bridging #? after)
    blobPairToApiBlobPair (Data.Inserting after)      = API.BlobPair Nothing (bridging #? after)
    blobPairToApiBlobPair (Data.Deleting before)      = API.BlobPair (bridging #? before) Nothing
