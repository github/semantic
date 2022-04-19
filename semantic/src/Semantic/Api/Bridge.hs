{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
module Semantic.Api.Bridge
  ( APIBridge (..)
  , APIConvert (..)
  , (#?)
  ) where

import           Analysis.File
import           Analysis.Reference
import           Control.Lens
import qualified Data.Blob as Data
import           Data.Either
import           Data.ProtoLens (defMessage)
import qualified Data.Text as T
import           Data.Text.Lens
import qualified Proto.Semantic as API
import           Proto.Semantic_Fields as P hiding (to)
import qualified Source.Language as Data
import qualified Source.Range as Source
import qualified Source.Source as Source (fromText, toText, totalSpan)
import qualified Source.Span as Source
import qualified System.Path as Path

-- | An @APIBridge x y@ instance describes an isomorphism between @x@ and @y@.
-- This is suitable for types such as 'Pos' which are representationally equivalent
-- in their API, legacy, and native forms. All 'Lens' laws apply.
--
-- Foreign to native: @x ^. bridging@
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

instance APIBridge API.Position Source.Pos where
  bridging = iso fromAPI toAPI where
    toAPI Source.Pos{..}     = defMessage & P.line .~ fromIntegral line & P.column .~ fromIntegral column
    fromAPI position = Source.Pos (fromIntegral (position ^. line)) (fromIntegral (position ^. column))

instance APIConvert API.Span Source.Span where
  converting = prism' toAPI fromAPI where
    toAPI Source.Span{..} = defMessage & P.maybe'start .~ (bridging #? start) & P.maybe'end .~ (bridging #? end)
    fromAPI span = Source.Span <$> (span ^. maybe'start >>= preview bridging) <*> (span ^. maybe'end >>= preview bridging)

instance APIBridge API.ByteRange Source.Range where
  bridging = iso fromAPI toAPI where
    toAPI Source.Range{..} = defMessage & P.start .~ fromIntegral start & P.end .~ fromIntegral end
    fromAPI range = Source.Range (fromIntegral (range ^. start)) (fromIntegral (range ^. end))

instance APIBridge T.Text Data.Language where
  bridging = iso Data.textToLanguage Data.languageToText

instance APIBridge API.Blob Data.Blob where
  bridging = iso apiBlobToBlob blobToApiBlob where
    blobToApiBlob b
      = defMessage
      & P.content .~ Source.toText (Data.blobSource b)
      & P.path .~ T.pack (Data.blobFilePath b)
      & P.language .~ (bridging # Data.blobLanguage b)
    apiBlobToBlob blob =
      let src = blob ^. content.to Source.fromText
          pth = fromRight (Path.toAbsRel Path.emptyFile) (blob ^. path._Text.to Path.parse)
      in Data.Blob
      { blobSource = src
      , blobFile = File (Reference pth (Source.totalSpan src)) (blob ^. language.bridging)
      }
