{-# LANGUAGE GADTs, TypeOperators, DerivingStrategies #-}
module Semantic.API.TOCSummaries (diffSummary) where

import Analysis.TOCSummary (Declaration, declarationAlgebra)
import Data.Blob
import Data.Diff
import Rendering.TOC
import Semantic.API.Converters
import Semantic.API.Diff
import Semantic.API.Types
import Semantic.Task as Task

diffSummary :: (DiffEffects sig m) => [BlobPair] -> m DiffTreeTOCResponse
diffSummary = distributeFoldMap go
  where
    go :: (DiffEffects sig m) => BlobPair -> m DiffTreeTOCResponse
    go blobPair = doDiff blobPair (decorate . declarationAlgebra) render

    render :: (Foldable syntax, Functor syntax, Applicative m) => BlobPair -> Diff syntax (Maybe Declaration) (Maybe Declaration) -> m DiffTreeTOCResponse
    render _ diff = pure $ foldr (\x acc -> acc <> toResponse x) mempty (diffTOC diff)
      where
        toResponse :: TOCSummary -> DiffTreeTOCResponse
        toResponse TOCSummary{..}   = DiffTreeTOCResponse [TOCSummaryChange summaryCategoryName summaryTermName (spanToSpan summarySpan) summaryChangeType] mempty
        toResponse ErrorSummary{..} = DiffTreeTOCResponse mempty [TOCSummaryError errorText (spanToSpan errorSpan)]
