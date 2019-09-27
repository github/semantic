{-# LANGUAGE GADTs, TypeOperators, DerivingStrategies, LambdaCase #-}
module Semantic.Api.TOCSummaries (diffSummary, legacyDiffSummary, diffSummaryBuilder) where

import           Analysis.TOCSummary (Declaration, declarationAlgebra)
import           Control.Effect.Error
import           Data.Aeson
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Diff
import qualified Data.Map.Monoidal as Map
import qualified Data.Text as T
import           Data.Semilattice.Lower
import           Rendering.TOC
import           Semantic.Api.Diffs
import           Semantic.Api.Bridge
import           Semantic.Task as Task
import           Serializing.Format

import           Control.Lens
import           Data.ProtoLens (defMessage)
import           Proto.Semantic as P hiding (Blob, BlobPair)
import           Proto.Semantic_Fields as P

diffSummaryBuilder :: (DiffEffects sig m) => Format DiffTreeTOCResponse -> [BlobPair] -> m Builder
diffSummaryBuilder format blobs = diffSummary blobs >>= serialize format

legacyDiffSummary :: (DiffEffects sig m) => [BlobPair] -> m Summaries
legacyDiffSummary = distributeFoldMap go
  where
    go :: (DiffEffects sig m) => BlobPair -> m Summaries
    go blobPair = doDiff blobPair (decorate . declarationAlgebra) render
      `catchError` \(SomeException e) ->
        pure $ Summaries mempty (Map.singleton path [toJSON (ErrorSummary (T.pack (show e)) lowerBound lang)])
      where path = T.pack $ pathKeyForBlobPair blobPair
            lang = languageForBlobPair blobPair

    render :: (Foldable syntax, Functor syntax, Applicative m) => BlobPair -> Diff syntax (Maybe Declaration) (Maybe Declaration) -> m Summaries
    render blobPair = pure . renderToCDiff blobPair

diffSummary :: (DiffEffects sig m) => [BlobPair] -> m DiffTreeTOCResponse
diffSummary blobs = do
  diff <- distributeFor blobs go
  pure $ defMessage & P.files .~ diff
  where
    go :: (DiffEffects sig m) => BlobPair -> m TOCSummaryFile
    go blobPair = doDiff blobPair (decorate . declarationAlgebra) render
      `catchError` \(SomeException e) ->
        pure $ defMessage
          & P.path .~ path
          & P.language .~ lang
          & P.changes .~ mempty
          & P.errors .~ [defMessage & P.error .~ T.pack (show e) & P.maybe'span .~ Nothing]
      where path = T.pack $ pathKeyForBlobPair blobPair
            lang = bridging # languageForBlobPair blobPair

    render :: (Foldable syntax, Functor syntax, Applicative m) => BlobPair -> Diff syntax (Maybe Declaration) (Maybe Declaration) -> m TOCSummaryFile
    render blobPair diff = pure $ foldr go (defMessage & P.path .~ path & P.language .~ lang & P.changes .~ mempty & P.errors .~ mempty) (diffTOC diff)
      where
        path = T.pack $ pathKeyForBlobPair blobPair
        lang = bridging # languageForBlobPair blobPair

        toChangeType = \case
          "added" -> ADDED
          "modified" -> MODIFIED
          "removed" -> REMOVED
          _ -> NONE

        go :: TOCSummary -> TOCSummaryFile -> TOCSummaryFile
        go TOCSummary{..} file = defMessage
          & P.path .~ file^.P.path
          & P.language .~ file^.P.language
          & P.changes .~ (defMessage & P.category .~ summaryCategoryName & P.term .~ summaryTermName & P.maybe'span .~ (converting #? summarySpan) & P.changeType .~ toChangeType summaryChangeType) : file^.P.changes
          & P.errors .~ file^.P.errors

        go ErrorSummary{..} file = defMessage
          & P.path .~ file^.P.path
          & P.language .~ file^.P.language
          & P.changes .~ file^.P.changes
          & P.errors .~ (defMessage & P.error .~ errorText & P.maybe'span .~ converting #? errorSpan) : file^.P.errors
