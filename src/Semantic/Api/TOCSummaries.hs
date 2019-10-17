{-# LANGUAGE LambdaCase #-}
module Semantic.Api.TOCSummaries (diffSummary, legacyDiffSummary, diffSummaryBuilder) where

import           Control.Effect.Error
import           Control.Lens
import           Data.Aeson
import           Data.Blob
import           Data.ByteString.Builder
import qualified Data.List as List
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
    go blobPair = decoratingDiffWith summarizeDiffParsers decorateTerm (pure . uncurry Summaries . bimap toMap toMap . List.partition isValidSummary . summarizeDiff) blobPair
      `catchError` \(SomeException e) ->
        pure $ Summaries mempty (Map.singleton path [toJSON (ErrorSummary (T.pack (show e)) lowerBound lang)])
      where path = T.pack $ pathKeyForBlobPair blobPair
            lang = languageForBlobPair blobPair

            toMap [] = mempty
            toMap as = Map.singleton summaryKey (toJSON <$> as)
            summaryKey = T.pack $ pathKeyForBlobPair blobPair


diffSummary :: DiffEffects sig m => [BlobPair] -> m DiffTreeTOCResponse
diffSummary blobs = do
  diff <- distributeFor blobs go
  pure $ defMessage & P.files .~ diff
  where
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
              & P.changes .~ (defMessage & P.category .~ kind & P.term .~ ident & P.maybe'span .~ (converting #? span) & P.changeType .~ toChangeType changeType) : file^.P.changes
              & P.errors .~ file^.P.errors

            combine ErrorSummary{..} file = defMessage
              & P.path .~ file^.P.path
              & P.language .~ file^.P.language
              & P.changes .~ file^.P.changes
              & P.errors .~ (defMessage & P.error .~ errorText & P.maybe'span .~ converting #? errorSpan) : file^.P.errors
