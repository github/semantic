{-# LANGUAGE GADTs, TypeOperators, DerivingStrategies, LambdaCase #-}
module Semantic.Api.TOCSummaries (diffSummary, legacyDiffSummary, diffSummaryBuilder) where

import           Analysis.TOCSummary (Declaration, declarationAlgebra)
import           Control.Effect.Error
import           Control.Lens
import           Data.Aeson
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Diff
import qualified Data.Map.Monoidal as Map
import           Data.Span (emptySpan)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Rendering.TOC
import           Semantic.Api.Diffs
import           Semantic.Api.Bridge
import           Semantic.Api.V1.CodeAnalysisPB hiding (Blob, BlobPair)
import           Semantic.Task as Task
import           Serializing.Format

diffSummaryBuilder :: (DiffEffects sig m) => Format DiffTreeTOCResponse -> [BlobPair] -> m Builder
diffSummaryBuilder format blobs = diffSummary blobs >>= serialize format

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

diffSummary :: (DiffEffects sig m) => [BlobPair] -> m DiffTreeTOCResponse
diffSummary blobs = DiffTreeTOCResponse . V.fromList <$> distributeFor blobs go
  where
    go :: (DiffEffects sig m) => BlobPair -> m TOCSummaryFile
    go blobPair = doDiff blobPair (decorate . declarationAlgebra) render
      `catchError` \(SomeException e) ->
        pure $ TOCSummaryFile path lang mempty (V.fromList [TOCSummaryError (T.pack (show e)) Nothing])
      where path = T.pack $ pathKeyForBlobPair blobPair
            lang = bridging # languageForBlobPair blobPair

    render :: (Foldable syntax, Functor syntax, Applicative m) => BlobPair -> Diff syntax (Maybe Declaration) (Maybe Declaration) -> m TOCSummaryFile
    render blobPair diff = pure $ foldr go (TOCSummaryFile path lang mempty mempty) (diffTOC diff)
      where
        path = T.pack $ pathKeyForBlobPair blobPair
        lang = bridging # languageForBlobPair blobPair

        toChangeType = \case
          "added" -> Added
          "modified" -> Modified
          "removed" -> Removed
          _ -> None

        go :: TOCSummary -> TOCSummaryFile -> TOCSummaryFile
        go TOCSummary{..} TOCSummaryFile{..}
          = TOCSummaryFile path language (V.cons (TOCSummaryChange summaryCategoryName summaryTermName (summarySpan ^? re bridging) (toChangeType summaryChangeType)) changes) errors
        go ErrorSummary{..} TOCSummaryFile{..}
          = TOCSummaryFile path language changes (V.cons (TOCSummaryError errorText (errorSpan ^? re bridging)) errors)
