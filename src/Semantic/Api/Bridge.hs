{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Semantic.Api.Bridge
  ( APIBridge (..)
  , APIConvert (..)
  , (#?)
  ) where

import           Analysis.File
import           Analysis.Functor.Named (name_)
import qualified Analysis.Name as Name
import           Control.Lens
import qualified Data.Blob as Data
import qualified Data.Edit as Data
import           Data.Either
import           Data.Functor.Tagged
import           Data.Generics.Product
import qualified Data.IntMap.Strict as IM
import qualified Data.Language as Data
import           Data.ProtoLens (defMessage)
import qualified Data.Text as T
import           Data.Text.Lens
import qualified Data.Vector as Vector
import qualified Proto.Semantic as API
import qualified Proto.Semantic_Fields as P
import qualified Semantic.Api.LegacyTypes as Legacy
import qualified Source.Source as Source (fromText, toText, totalSpan)
import qualified Source.Span as Span
import qualified Source.Span as Source
import qualified Stack.File as Stack
import qualified Stack.Node as Stack
import qualified Stack.Path
import qualified System.Path as Path
import qualified Tags.Tag as Tag

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

instance APIConvert T.Text Tag.Kind where
  converting = prism' toAPI fromAPI where
    toAPI = T.pack . show
    fromAPI = \case
      "Function" -> Just Tag.Function
      "Method"   -> Just Tag.Method
      "Class" -> Just Tag.Class
      "Module" -> Just Tag.Module
      _ -> Nothing

instance APIConvert API.StackGraphNode'NodeType Stack.Type where
  converting = Control.Lens.from enum.enum


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

instance APIBridge API.StackGraphNode Stack.Node where
  bridging = iso apiNodeToNode nodeToApiNode where
    apiNodeToNode :: API.StackGraphNode -> Stack.Node
    apiNodeToNode s = Stack.Node
      (s ^. P.id.to fromIntegral)
      (s ^. P.name.to Name.name)
      (s ^. P.line)
      (s ^? P.kind.converting^.non Tag.Method)
      (s ^? P.span.converting^.non (Span.point (Span.Pos 0 0)))
      (s ^? P.nodeType.converting^.non Stack.Unknown)

    nodeToApiNode :: Stack.Node -> API.StackGraphNode
    nodeToApiNode node = defMessage
      & P.id .~ node ^. identifier.enum
      & P.name .~ node ^. contents.name_.to Name.formatName
      & P.line .~ node ^. Stack.info_.Stack.line_
      & P.kind .~ node ^. Stack.info_.Stack.kind_.re converting
      & P.span .~ node ^. Stack.info_.Span.span_.re converting
      & P.nodeType .~ node ^. Stack.info_.Stack.type_.re converting

stackDelimiter :: Char
stackDelimiter = '\x1E'

instance APIConvert API.StackGraphFile Stack.File where
  converting = iso fromApi toApi
   where
     fromApi :: API.StackGraphFile -> Stack.File
     fromApi s = Stack.File
       { Stack.path = fromRight (Path.toAbsRel Path.emptyFile) (Path.parse (s ^. P.path.to T.unpack))
       , Stack.language = s^.P.language
       , Stack.nodes = Vector.map (view bridging) ((s^.P.vec'nodes) :: Vector.Vector API.StackGraphNode)
       , Stack.paths =
         let
           lookupTable :: IM.IntMap Stack.Node
           lookupTable = foldMap (\a -> IM.singleton (a^.P.id.to fromIntegral) (a^.bridging)) (s^.P.vec'nodes)
           conv :: API.StackGraphPath -> Maybe Stack.Path.Path
           conv n = Stack.Path.Path
             <$> IM.lookup (n^.P.from.to fromIntegral) lookupTable
             <*> IM.lookup (n^.P.to.to fromIntegral) lookupTable
             <*> pure mempty
             <*> fmap (fmap Name.name) (n ^? P.startingSymbolStack)
             <*> fmap (fmap Name.name) (n ^? P.endingSymbolStack)
             <*> Just (n ^. P.startingScopeStackSize.to (toEnum . fromIntegral))
             <*> Just (fmap fromIntegral (view P.endingScopeStack n))
         in
           Vector.mapMaybe conv (s^.P.vec'paths)
       , Stack.errors = mempty
       }

     toApi :: Stack.File -> API.StackGraphFile
     toApi f = defMessage
       & P.path .~ f ^. field @"path".to Path.toString.from _Text
       & P.language .~ f ^. field @"language"


