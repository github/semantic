{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Semantic.Api.Bridge
  ( APIBridge (..)
  , APIConvert (..)
  , (#?)
  ) where

import           Analysis.File
import           Analysis.Name
import           Control.Lens
import           Data.Bifunctor
import qualified Data.Blob as Data
import           Data.Either
import           Data.Foldable
import           Data.Functor.Tagged
import           Data.Int
import qualified Data.IntMap.Strict as IM
import qualified Data.Language as Data
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe
import           Data.ProtoLens (defMessage)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Text.Lens
import qualified Data.Validation as Validation
import           Numeric.Lens
import qualified Proto.Semantic as API
import qualified Proto.Semantic_Fields as P
import qualified Semantic.Api.LegacyTypes as Legacy
import qualified Source.Source as Source (fromText, toText, totalSpan)
import qualified Source.Span as Source
import qualified Stack.Graph as SG
import qualified Stack.Path as SG
import qualified System.Path as Path

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
    fromAPI position = Source.Pos (fromIntegral (position^.P.line)) (fromIntegral (position^.P.column))

instance APIConvert API.Span Source.Span where
  converting = prism' toAPI fromAPI where
    toAPI Source.Span{..} = defMessage & P.maybe'start .~ (bridging #? start) & P.maybe'end .~ (bridging #? end)
    fromAPI span = Source.Span <$> (span^.P.maybe'start >>= preview bridging) <*> (span^.P.maybe'end >>= preview bridging)

instance APIConvert Legacy.Span Source.Span where
  converting = prism' toAPI fromAPI where
    toAPI Source.Span{..} = Legacy.Span (bridging #? start) (bridging #? end)
    fromAPI Legacy.Span {..} = Source.Span <$> (start >>= preview bridging) <*> (end >>= preview bridging)

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
      let src = blob^.P.content.to Source.fromText
          pth = fromRight (Path.toAbsRel Path.emptyFile) (blob^.P.path._Text.to Path.parse)
      in Data.Blob
      { blobSource = src
      , blobFile = File pth (Source.totalSpan src) (blob^.P.language.bridging)
      }
