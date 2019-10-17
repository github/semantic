{-# LANGUAGE LambdaCase #-}
module Semantic.Api.TOCSummaries (diffSummary, legacyDiffSummary, diffSummaryBuilder) where

import           Control.Effect.Error
import           Control.Lens
import           Data.Aeson
import           Data.Blob
import           Data.ByteString.Builder
import qualified Data.Map.Monoidal as Map
import           Data.ProtoLens (defMessage)
import           Data.Semilattice.Lower
import qualified Data.Text as T
import           Proto.Semantic as P hiding (Blob, BlobPair)
import           Proto.Semantic_Fields as P
import           Rendering.TOC
import           Semantic.Api.Bridge
import           Semantic.Api.Diffs
import           Semantic.Task as Task
import           Serializing.Format

diffSummaryBuilder :: DiffEffects sig m => Format DiffTreeTOCResponse -> [BlobPair] -> m Builder
diffSummaryBuilder format blobs = diffSummary blobs >>= serialize format

legacyDiffSummary :: DiffEffects sig m => [BlobPair] -> m Summaries
legacyDiffSummary = distributeFoldMap go
  where
    go :: DiffEffects sig m => BlobPair -> m Summaries
    go blobPair = decoratingDiffWith legacySummarizeDiffParsers legacyDecorateTerm (pure . legacySummarizeDiff blobPair) blobPair
      `catchError` \(SomeException e) ->
        pure $ Summaries mempty (Map.singleton path [toJSON (ErrorSummary (T.pack (show e)) lowerBound lang)])
      where path = T.pack $ pathKeyForBlobPair blobPair
            lang = languageForBlobPair blobPair


diffSummary :: DiffEffects sig m => [BlobPair] -> m DiffTreeTOCResponse
diffSummary blobs = do
  diff <- distributeFor blobs go
  pure $ defMessage & P.files .~ diff
  where
    go :: DiffEffects sig m => BlobPair -> m TOCSummaryFile
    go blobPair = decoratingDiffWith summarizeDiffParsers decorateTerm (pure . foldr combine (defMessage & P.path .~ path & P.language .~ lang) . summarizeDiff) blobPair
      `catchError` \(SomeException e) ->
        pure $ defMessage
          & P.path .~ path
          & P.language .~ lang
          & P.changes .~ mempty
          & P.errors .~ [defMessage & P.error .~ T.pack (show e) & P.maybe'span .~ Nothing]
      where path = T.pack $ pathKeyForBlobPair blobPair
            lang = bridging # languageForBlobPair blobPair

            toChangeType = \case
              "added" -> ADDED
              "modified" -> MODIFIED
              "removed" -> REMOVED
              _ -> NONE

            combine :: TOCSummary -> TOCSummaryFile -> TOCSummaryFile
            combine TOCSummary{..} file = defMessage
              & P.path .~ file^.P.path
              & P.language .~ file^.P.language
              & P.changes .~ (defMessage & P.category .~ summaryCategoryName & P.term .~ summaryTermName & P.maybe'span .~ (converting #? summarySpan) & P.changeType .~ toChangeType summaryChangeType) : file^.P.changes
              & P.errors .~ file^.P.errors

            combine ErrorSummary{..} file = defMessage
              & P.path .~ file^.P.path
              & P.language .~ file^.P.language
              & P.changes .~ file^.P.changes
              & P.errors .~ (defMessage & P.error .~ errorText & P.maybe'span .~ converting #? errorSpan) : file^.P.errors
