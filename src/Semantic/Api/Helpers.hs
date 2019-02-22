{-# LANGUAGE LambdaCase #-}
module Semantic.Api.Helpers
  ( spanToSpan
  , spanToLegacySpan
  , toChangeType
  , languageToApiLanguage
  , apiLanguageToLanguage
  , apiBlobsToBlobs
  , apiBlobToBlob
  , apiBlobPairsToBlobPairs
  , apiBlobPairToBlobPair
  ) where

import           Data.Bifunctor.Join
import qualified Data.Blob as Data
import qualified Data.Language as Data
import           Data.Source (fromText)
import qualified Data.Span as Data
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.These
import qualified Semantic.Api.LegacyTypes as Legacy
import qualified Semantic.Api.V1.CodeAnalysisPB as API

spanToSpan :: Data.Span -> Maybe API.Span
spanToSpan Data.Span{..} = Just $ API.Span (toPos spanStart) (toPos spanEnd)
  where toPos Data.Pos{..} = Just $ API.Position (fromIntegral posLine) (fromIntegral posColumn)

spanToLegacySpan :: Data.Span -> Maybe Legacy.Span
spanToLegacySpan Data.Span{..} = Just $ Legacy.Span (toPos spanStart) (toPos spanEnd)
  where toPos Data.Pos{..} = Just $ Legacy.Position posLine posColumn

toChangeType :: T.Text -> API.ChangeType
toChangeType = \case
  "added" -> API.Added
  "modified" -> API.Modified
  "removed" -> API.Removed
  _ -> API.None

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

apiBlobsToBlobs :: V.Vector API.Blob -> [Data.Blob]
apiBlobsToBlobs = V.toList . fmap apiBlobToBlob

apiBlobToBlob :: API.Blob -> Data.Blob
apiBlobToBlob API.Blob{..} = Data.Blob (fromText content) (T.unpack path) (apiLanguageToLanguage language)

apiBlobPairsToBlobPairs :: V.Vector API.BlobPair -> [Data.BlobPair]
apiBlobPairsToBlobPairs = V.toList . fmap apiBlobPairToBlobPair

apiBlobPairToBlobPair :: API.BlobPair -> Data.BlobPair
apiBlobPairToBlobPair (API.BlobPair (Just before) (Just after)) = Join (These (apiBlobToBlob before) (apiBlobToBlob after))
apiBlobPairToBlobPair (API.BlobPair (Just before) Nothing) = Join (This (apiBlobToBlob before))
apiBlobPairToBlobPair (API.BlobPair Nothing (Just after)) = Join (That (apiBlobToBlob after))
apiBlobPairToBlobPair _ = Prelude.error "Expected BlobPair to have either 'before' and/or 'after'."
