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
  , pathToApiPath
  , apiPathToPath
  ) where

import           Analysis.File
import           Analysis.Name
import           Control.Lens
import qualified Data.Blob as Data
import qualified Data.Edit as Data
import           Data.Either
import           Data.Functor.Tagged
import           Data.Int
import qualified Data.IntMap.Strict as IM
import qualified Data.Language as Data
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe
import           Data.ProtoLens (defMessage)
import           Data.Sequence (Seq)
import qualified Data.Text as T
import           Data.Text.Lens
import qualified Data.Validation as Validation
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
      & P.path .~ T.pack (Data.blobPath b)
      & P.language .~ (bridging # Data.blobLanguage b)
    apiBlobToBlob blob =
      let src = blob^.P.content.to Source.fromText
          pth = fromRight (Path.toAbsRel Path.emptyFile) (blob^.P.path._Text.to Path.parse)
      in Data.Blob
      { blobSource = src
      , blobFile = File pth (Source.totalSpan src) (blob^.P.language.bridging)
      }

instance APIConvert API.BlobPair Data.BlobPair where
  converting = prism' blobPairToApiBlobPair apiBlobPairToBlobPair where

    apiBlobPairToBlobPair blobPair = case (blobPair^.P.maybe'before, blobPair^.P.maybe'after) of
      (Just before, Just after) -> Just $ Data.Compare (before^.bridging) (after^.bridging)
      (Just before, Nothing)    -> Just $ Data.Delete (before^.bridging)
      (Nothing, Just after)     -> Just $ Data.Insert (after^.bridging)
      _                         -> Nothing

    blobPairToApiBlobPair (Data.Compare before after) = defMessage & P.maybe'before .~ (bridging #? before) & P.maybe'after .~ (bridging #? after)
    blobPairToApiBlobPair (Data.Insert after)         = defMessage & P.maybe'before .~ Nothing & P.maybe'after .~ (bridging #? after)
    blobPairToApiBlobPair (Data.Delete before)        = defMessage & P.maybe'before .~ (bridging #? before) & P.maybe'after .~ Nothing

-- * Stack graph stuff. This doesn't fit into the optics formulation because it relies on
-- contextual information

data PathConvertError
  = NodeNotFound Tag
  | InvalidStartingSize Int64
  | InvariantViolated SG.PathInvariantError

type NodeCache = IM.IntMap (Tagged SG.Node)



stackDelimiter :: T.Text
stackDelimiter = T.pack "\x1E"

pathToApiPath :: SG.Path -> API.StackGraphPath
pathToApiPath path =
  defMessage
    & P.from .~ path^.SG.startingNode_.identifier
    & P.to .~ path^.SG.endingNode_.identifier
    & P.endingScopeStack .~ path^.SG.endingScopeStack_
    & P.startingSymbolStack .~ fmap formatName (path^.SG.startingSymbolStack_)
    & P.endingSymbolStack .~ fmap formatName (path^.SG.endingSymbolStack_)
    & P.startingScopeStackSize .~ (if path^.SG.startingScopeStackSize_ == SG.One then 1 else 0)
    & P.edges .~ (path^.SG.edgeLabels_)

apiPathToPath :: Seq SG.Edge -> NodeCache -> API.StackGraphPath -> Either (NonEmpty PathConvertError) SG.Path
apiPathToPath edges nc path = Validation.toEither validate >>= ensureInvariantsHold
  where
    validate = do
      let failing = Validation.Failure . pure @NonEmpty @PathConvertError
          lookupNode n = case IM.lookup (fromIntegral n) nc of
            Just a  -> pure a
            Nothing -> failing (NodeNotFound n)

      startingNode <- lookupNode (path^.P.from)
      endingNode <- lookupNode (path^.P.to)
      endingScopeStack <- pure (path^.P.endingScopeStack)
      edgeLabels <- pure . view P.edges $ path
      startingSymbolStack <- pure . fmap name . view P.startingSymbolStack $ path
      endingSymbolStack <- pure . fmap name . view P.startingSymbolStack $ path
      startingScopeStackSize <- case path^.P.startingScopeStackSize of
        0 -> pure SG.Zero
        1 -> pure SG.One
        n -> failing (InvalidStartingSize n)
      pure SG.Path{..}
    ensureInvariantsHold n =
      let allErrors = fmap InvariantViolated (catMaybes [SG.checkEdgeInvariants edges n, SG.checkNodeInvariants n])
      in case allErrors of
        []   -> pure n
        x:xs -> Left (x :| xs)
