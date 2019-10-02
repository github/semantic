{-# LANGUAGE GADTs, TypeOperators, DerivingStrategies, LambdaCase #-}
module Semantic.Api.TOCSummaries (diffSummary, legacyDiffSummary, diffSummaryBuilder) where

import           Analysis.Decorator (decoratorWithAlgebra)
import           Analysis.TOCSummary (Declaration, declarationAlgebra)
import           Control.Effect.Error
import           Control.Lens
import           Data.Aeson
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Diff
import qualified Data.Map.Monoidal as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Semilattice.Lower
import           Rendering.TOC
import           Semantic.Api.Diffs
import           Semantic.Api.Bridge
import           Semantic.Proto.SemanticPB hiding (Blob, BlobPair)
import           Semantic.Task as Task
import           Serializing.Format

diffSummaryBuilder :: DiffEffects sig m => Format DiffTreeTOCResponse -> [BlobPair] -> m Builder
diffSummaryBuilder format blobs = diffSummary blobs >>= serialize format

legacyDiffSummary :: DiffEffects sig m => [BlobPair] -> m Summaries
legacyDiffSummary = distributeFoldMap go
  where
    go :: DiffEffects sig m => BlobPair -> m Summaries
    go blobPair = doDiff (\ blob -> decoratorWithAlgebra (declarationAlgebra blob)) (pure . legacySummarizeDiff blobPair) blobPair
      `catchError` \(SomeException e) ->
        pure $ Summaries mempty (Map.singleton path [toJSON (ErrorSummary (T.pack (show e)) lowerBound lang)])
      where path = T.pack $ pathKeyForBlobPair blobPair
            lang = languageForBlobPair blobPair

class LegacySummarizeDiff diff where
  legacySummarizeDiff :: BlobPair -> diff (Maybe Declaration) (Maybe Declaration) -> Summaries

instance (Foldable syntax, Functor syntax) => LegacySummarizeDiff (Diff syntax) where
  legacySummarizeDiff = renderToCDiff

diffSummary :: DiffEffects sig m => [BlobPair] -> m DiffTreeTOCResponse
diffSummary blobs = DiffTreeTOCResponse . V.fromList <$> distributeFor blobs go
  where
    go :: DiffEffects sig m => BlobPair -> m TOCSummaryFile
    go blobPair = doDiff (\ blob -> decoratorWithAlgebra (declarationAlgebra blob)) (pure . summarizeDiff blobPair) blobPair
      `catchError` \(SomeException e) ->
        pure $ TOCSummaryFile path lang mempty (V.fromList [TOCSummaryError (T.pack (show e)) Nothing])
      where path = T.pack $ pathKeyForBlobPair blobPair
            lang = bridging # languageForBlobPair blobPair

class SummarizeDiff diff where
  summarizeDiff :: BlobPair -> diff (Maybe Declaration) (Maybe Declaration) -> TOCSummaryFile

instance (Foldable syntax, Functor syntax) => SummarizeDiff (Diff syntax) where
  summarizeDiff blobPair diff = foldr go (TOCSummaryFile path lang mempty mempty) (diffTOC diff)
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
        = TOCSummaryFile path language (V.cons (TOCSummaryChange summaryCategoryName summaryTermName (converting #? summarySpan) (toChangeType summaryChangeType)) changes) errors
      go ErrorSummary{..} TOCSummaryFile{..}
        = TOCSummaryFile path language changes (V.cons (TOCSummaryError errorText (converting #? errorSpan)) errors)
