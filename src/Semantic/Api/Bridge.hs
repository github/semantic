{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, RecordWildCards #-}
module Semantic.Api.Bridge
  ( APIBridge (..)
  , APIConvert (..)
  , (#?)
  ) where

import           Control.Lens
import qualified Data.Blob as Data
import qualified Data.Edit as Data
import qualified Data.Language as Data
import           Data.ProtoLens (defMessage)
import qualified Data.Text as T
import qualified Semantic.Api.LegacyTypes as Legacy
import qualified Proto.Semantic as API
import           Proto.Semantic_Fields as P
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
    toAPI Source.Pos{..}     = defMessage & P.line .~ fromIntegral line & P.column .~ fromIntegral column
    fromAPI position = Source.Pos (fromIntegral (position^.line)) (fromIntegral (position^.column))

instance APIConvert API.Span Source.Span where
  converting = prism' toAPI fromAPI where
    toAPI Source.Span{..} = defMessage & P.maybe'start .~ (bridging #? start) & P.maybe'end .~ (bridging #? end)
    fromAPI span = Source.Span <$> (span^.maybe'start >>= preview bridging) <*> (span^.maybe'end >>= preview bridging)

instance APIConvert Legacy.Span Source.Span where
  converting = prism' toAPI fromAPI where
    toAPI Source.Span{..} = Legacy.Span (bridging #? start) (bridging #? end)
    fromAPI Legacy.Span {..} = Source.Span <$> (start >>= preview bridging) <*> (end >>= preview bridging)

instance APIBridge T.Text Data.Language where
  bridging = iso Data.textToLanguage Data.languageToText

instance APIBridge API.Blob Data.Blob where
  bridging = iso apiBlobToBlob blobToApiBlob where
    blobToApiBlob b = defMessage & P.content .~ toText (Data.blobSource b) & P.path .~ T.pack (Data.blobPath b) & P.language .~ (bridging # Data.blobLanguage b)
    apiBlobToBlob blob = Data.makeBlob (fromText (blob^.content)) (T.unpack (blob^.path)) (blob^.(language . bridging)) mempty


instance APIConvert API.BlobPair Data.BlobPair where
  converting = prism' blobPairToApiBlobPair apiBlobPairToBlobPair where

    apiBlobPairToBlobPair blobPair = case (blobPair^.maybe'before, blobPair^.maybe'after) of
      (Just before, Just after) -> Just $ Data.Compare (before^.bridging) (after^.bridging)
      (Just before, Nothing)    -> Just $ Data.Delete (before^.bridging)
      (Nothing, Just after)     -> Just $ Data.Insert (after^.bridging)
      _                         -> Nothing

    blobPairToApiBlobPair (Data.Compare before after) = defMessage & P.maybe'before .~ (bridging #? before) & P.maybe'after .~ (bridging #? after)
    blobPairToApiBlobPair (Data.Insert after)         = defMessage & P.maybe'before .~ Nothing & P.maybe'after .~ (bridging #? after)
    blobPairToApiBlobPair (Data.Delete before)        = defMessage & P.maybe'before .~ (bridging #? before) & P.maybe'after .~ Nothing
