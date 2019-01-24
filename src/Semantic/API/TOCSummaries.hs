{-# LANGUAGE GADTs, TypeOperators, DerivingStrategies #-}
module Semantic.API.TOCSummaries (diffSummary, legacyDiffSummary, diffSummaryBuilder) where

import Analysis.TOCSummary (Declaration, declarationAlgebra)
import Data.Blob
import Data.ByteString.Builder
import Data.Diff
import Rendering.TOC
import Semantic.API.Converters
import Semantic.API.Diffs
import Semantic.API.Types
import Semantic.Task as Task
import Serializing.Format
import qualified Data.Text as T

-- TODO: Switch away from legacy format on CLI too.
-- diffSummaryBuilder :: (DiffEffects sig m) => Format DiffTreeTOCResponse -> [BlobPair] -> m Builder
-- diffSummaryBuilder format blobs = runSerialize Plain format <$> diffSummary blobs
diffSummaryBuilder :: (DiffEffects sig m) => Format Summaries -> [BlobPair] -> m Builder
diffSummaryBuilder format blobs = runSerialize Plain format <$> legacyDiffSummary blobs

legacyDiffSummary :: (DiffEffects sig m) => [BlobPair] -> m Summaries
legacyDiffSummary = distributeFoldMap go
  where
    go :: (DiffEffects sig m) => BlobPair -> m Summaries
    go blobPair = doDiff blobPair (decorate . declarationAlgebra) render

    render :: (Foldable syntax, Functor syntax, Applicative m) => BlobPair -> Diff syntax (Maybe Declaration) (Maybe Declaration) -> m Summaries
    render blobPair = pure . renderToCDiff blobPair

diffSummary :: (DiffEffects sig m) => [BlobPair] -> m DiffTreeTOCResponse
diffSummary blobs = DiffTreeTOCResponse <$> distributeFor blobs go
  where
    go :: (DiffEffects sig m) => BlobPair -> m TOCSummaryFile
    go blobPair = doDiff blobPair (decorate . declarationAlgebra) render

    render :: (Foldable syntax, Functor syntax, Applicative m) => BlobPair -> Diff syntax (Maybe Declaration) (Maybe Declaration) -> m TOCSummaryFile
    render blobPair diff = pure $ foldr go (TOCSummaryFile path lang mempty mempty) (diffTOC diff)
      where
        path = T.pack $ pathKeyForBlobPair blobPair
        lang = languageForBlobPair blobPair

        go :: TOCSummary -> TOCSummaryFile -> TOCSummaryFile
        go TOCSummary{..} TOCSummaryFile{..}
          = TOCSummaryFile path language (TOCSummaryChange summaryCategoryName summaryTermName (spanToSpan summarySpan) summaryChangeType : changes) errors
        go ErrorSummary{..} TOCSummaryFile{..}
          = TOCSummaryFile path language changes (TOCSummaryError errorText (spanToSpan errorSpan) : errors)
