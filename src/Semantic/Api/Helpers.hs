{-# LANGUAGE FunctionalDependencies, LambdaCase, MultiParamTypeClasses #-}
module Semantic.Api.Helpers
  ( APIBridge (..)
  , toChangeType
  , apiBlobsToBlobs
  ) where

import Prologue

import           Control.Lens
import qualified Data.Blob as Data
import qualified Data.Language as Data
import           Data.Source (fromText, toText)
import qualified Data.Span as Data
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Semantic.Api.LegacyTypes as Legacy
import qualified Semantic.Api.V1.CodeAnalysisPB as API

class APIConvert api native | api -> native where
  converting :: Prism' api native

class APIBridge api native | api -> native where
  bridging :: Iso' api native

instance APIBridge Legacy.Position Data.Pos where
  bridging = iso fromAPI toAPI where
    toAPI Data.Pos{..}          = Legacy.Position posLine posColumn
    fromAPI Legacy.Position{..} = Data.Pos line column

instance APIBridge API.Position Data.Pos where
  bridging = iso fromAPI toAPI where
    toAPI Data.Pos{..}          = API.Position (fromIntegral posLine) (fromIntegral posColumn)
    fromAPI API.Position{..}    = Data.Pos (fromIntegral line) (fromIntegral column)

instance APIBridge Data.Span API.Span where
  bridging = iso toAPI fromAPI where
    toAPI Data.Span{..} = API.Span (spanStart ^? re bridging) (spanEnd ^? re bridging)
    fromAPI API.Span{..} = Data.Span (start^.non single.bridging) (end^.non single.bridging)
    single = API.Position 1 1

instance APIBridge API.Language Data.Language where
  bridging = iso apiLanguageToLanguage languageToApiLanguage where
    languageToApiLanguage :: Data.Language -> API.Language
    languageToApiLanguage = \case
      Data.Unknown -> API.Unknown
      Data.Go -> API.Go
      Data.Haskell -> API.Haskell
      Data.Java -> API.Java
      Data.JavaScript -> API.Javascript
      Data.JSON -> API.Json
      Data.JSX -> API.Jsx
      Data.Markdown -> API.Markdown
      Data.Python -> API.Python
      Data.Ruby -> API.Ruby
      Data.TypeScript -> API.Typescript
      Data.PHP -> API.Php

    apiLanguageToLanguage :: API.Language -> Data.Language
    apiLanguageToLanguage = \case
      API.Unknown -> Data.Unknown
      API.Go -> Data.Go
      API.Haskell -> Data.Haskell
      API.Java -> Data.Java
      API.Javascript -> Data.JavaScript
      API.Json -> Data.JSON
      API.Jsx -> Data.JSX
      API.Markdown -> Data.Markdown
      API.Python -> Data.Python
      API.Ruby -> Data.Ruby
      API.Typescript -> Data.TypeScript
      API.Php -> Data.PHP

instance APIBridge API.Blob Data.Blob where
  bridging = iso apiBlobToBlob blobToApiBlob where
    blobToApiBlob Data.Blob{..} = API.Blob (toText blobSource) (T.pack blobPath) (bridging # blobLanguage)
    apiBlobToBlob API.Blob{..} = Data.Blob (fromText content) (T.unpack path) (language ^. bridging)


instance APIConvert API.BlobPair Data.BlobPair where
  converting = prism' blobPairToApiBlobPair apiBlobPairToBlobPair where

    apiBlobPairToBlobPair (API.BlobPair (Just before) (Just after)) = Just $ Data.Diffing (before^.bridging) (after^.bridging)
    apiBlobPairToBlobPair (API.BlobPair (Just before) Nothing) = Just $ Data.Deleting (before^.bridging)
    apiBlobPairToBlobPair (API.BlobPair Nothing (Just after)) = Just $ Data.Inserting (after^.bridging)
    apiBlobPairToBlobPair _ = Nothing


    blobPairToApiBlobPair (Data.Diffing before after) = API.BlobPair (before ^? re bridging) (after ^? re bridging)
    blobPairToApiBlobPair (Data.Inserting after)      = API.BlobPair Nothing (after ^? re bridging)
    blobPairToApiBlobPair (Data.Deleting before)      = API.BlobPair (before ^? re bridging) Nothing

toChangeType :: T.Text -> API.ChangeType
toChangeType = \case
  "added" -> API.Added
  "modified" -> API.Modified
  "removed" -> API.Removed
  _ -> API.None

apiBlobsToBlobs :: V.Vector API.Blob -> [Data.Blob]
apiBlobsToBlobs = V.toList . fmap (^.bridging)
