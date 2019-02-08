{-# LANGUAGE GADTs, TypeOperators, DerivingStrategies #-}
module Semantic.API.TOCSummaries (diffSummary, legacyDiffSummary, diffSummaryBuilder) where

import           Analysis.TOCSummary (Declaration, declarationAlgebra)
import           Control.Effect.Error
import           Data.Aeson
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Diff
import qualified Data.Map.Monoidal as Map
import           Data.Span (emptySpan)
import qualified Data.Text as T
import           Rendering.TOC
import           Semantic.API.Diffs
import           Semantic.API.Helpers
import           Semantic.Api.V1.CodeAnalysisPB hiding (Blob, BlobPair)
import qualified Semantic.Api.V1.CodeAnalysisPB as API
import           Semantic.Task as Task
import           Serializing.Format

diffSummaryBuilder :: (DiffEffects sig m) => Format Summaries -> [BlobPair] -> m Builder
diffSummaryBuilder format blobs
  -- TODO: Switch away from legacy format on CLI too.
  = legacyDiffSummary blobs >>= serialize format

legacyDiffSummary :: (DiffEffects sig m) => [BlobPair] -> m Summaries
legacyDiffSummary = distributeFoldMap go
  where
    go :: (DiffEffects sig m) => BlobPair -> m Summaries
    go blobPair = doDiff blobPair (decorate . declarationAlgebra) render
      `catchError` \(SomeException e) ->
        pure $ Summaries mempty (Map.singleton path [toJSON (ErrorSummary (T.pack (show e)) emptySpan lang)])
      where path = T.pack $ pathKeyForBlobPair blobPair
            lang = languageForBlobPair blobPair

    render :: (Foldable syntax, Functor syntax, Applicative m) => BlobPair -> Diff syntax (Maybe Declaration) (Maybe Declaration) -> m Summaries
    render blobPair = pure . renderToCDiff blobPair

diffSummary :: (DiffEffects sig m) => [API.BlobPair] -> m DiffTreeTOCResponse
diffSummary blobs = DiffTreeTOCResponse <$> distributeFor (apiBlobPairToBlobPair <$> blobs) go
  where
    go :: (DiffEffects sig m) => BlobPair -> m TOCSummaryFile
    go blobPair = doDiff blobPair (decorate . declarationAlgebra) render
      `catchError` \(SomeException e) ->
        pure $ TOCSummaryFile path lang mempty [TOCSummaryError (T.pack (show e)) Nothing]
      where path = T.pack $ pathKeyForBlobPair blobPair
            lang = languageToApiLanguage $ languageForBlobPair blobPair

    render :: (Foldable syntax, Functor syntax, Applicative m) => BlobPair -> Diff syntax (Maybe Declaration) (Maybe Declaration) -> m TOCSummaryFile
    render blobPair diff = pure $ foldr go (TOCSummaryFile path lang mempty mempty) (diffTOC diff)
      where
        path = T.pack $ pathKeyForBlobPair blobPair
        lang = languageToApiLanguage $ languageForBlobPair blobPair

        go :: TOCSummary -> TOCSummaryFile -> TOCSummaryFile
        go TOCSummary{..} TOCSummaryFile{..}
          = TOCSummaryFile path language (TOCSummaryChange summaryCategoryName summaryTermName (spanToSpan summarySpan) (toChangeType summaryChangeType) : changes) errors
        go ErrorSummary{..} TOCSummaryFile{..}
          = TOCSummaryFile path language changes (TOCSummaryError errorText (spanToSpan errorSpan) : errors)
