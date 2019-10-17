{-# LANGUAGE LambdaCase #-}
module Semantic.Api.TOCSummaries (diffSummary, legacyDiffSummary, diffSummaryBuilder) where

import           Control.Effect.Error
import           Control.Lens
import           Data.Aeson
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Either (partitionEithers)
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
    go blobPair = decoratingDiffWith summarizeDiffParsers decorateTerm (pure . uncurry (flip Summaries) . bimap toMap toMap . partitionEithers . summarizeDiff) blobPair
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
    go blobPair = decoratingDiffWith summarizeDiffParsers decorateTerm (pure . uncurry toFile . partitionEithers . map (bimap toError toChange) . summarizeDiff) blobPair
      `catchError` \(SomeException e) ->
        pure $ toFile [defMessage & P.error .~ T.pack (show e) & P.maybe'span .~ Nothing] []
      where path = T.pack $ pathKeyForBlobPair blobPair
            lang = bridging # languageForBlobPair blobPair

            toFile errors changes = defMessage
              & P.path     .~ path
              & P.language .~ lang
              & P.changes  .~ changes
              & P.errors   .~ errors

            toChangeType = \case
              Changed  -> MODIFIED
              Deleted  -> REMOVED
              Inserted -> ADDED
              Replaced -> MODIFIED

            toChange :: TOCSummary -> TOCSummaryChange
            toChange TOCSummary{..} = defMessage & P.category .~ kind & P.term .~ ident & P.maybe'span .~ (converting #? span) & P.changeType .~ toChangeType change

            toError :: ErrorSummary -> TOCSummaryError
            toError ErrorSummary{..} = defMessage & P.error .~ message & P.maybe'span .~ converting #? span
