{-# LANGUAGE LambdaCase #-}
module Semantic.API.Helpers
  ( spanToSpan
  , spanToLegacySpan
  , toChangeType
  , apiBlobToBlob
  , apiBlobPairToBlobPair
  ) where

import qualified Semantic.API.Types as API
import qualified Semantic.API.LegacyTypes as Legacy
import qualified Data.Span as Data
import qualified Data.Text as T
import qualified Data.Blob as Data
import Data.Bifunctor.Join
import Data.These
import Data.Source (fromText)
import Data.Language (languageForScope)

spanToSpan :: Data.Span -> Maybe API.Span
spanToSpan Data.Span{..} = Just $ API.Span (toPos spanStart) (toPos spanEnd)
  where toPos Data.Pos{..} = Just $ API.Position posLine posColumn

spanToLegacySpan :: Data.Span -> Maybe Legacy.Span
spanToLegacySpan Data.Span{..} = Just $ Legacy.Span (toPos spanStart) (toPos spanEnd)
  where toPos Data.Pos{..} = Just $ Legacy.Position posLine posColumn

toChangeType :: T.Text -> API.ChangeType
toChangeType = \case
  "added" -> API.Added
  "modified" -> API.Modified
  "removed" -> API.Removed
  _ -> API.None

apiBlobToBlob :: API.Blob -> Data.Blob
apiBlobToBlob API.Blob{..} = Data.Blob (fromText content) path (languageForScope scope)

apiBlobPairToBlobPair :: API.BlobPair -> Data.BlobPair
apiBlobPairToBlobPair (API.BlobPair (Just before) (Just after)) = Join (These (apiBlobToBlob before) (apiBlobToBlob after))
apiBlobPairToBlobPair (API.BlobPair (Just before) Nothing) = Join (This (apiBlobToBlob before))
apiBlobPairToBlobPair (API.BlobPair Nothing (Just after)) = Join (That (apiBlobToBlob after))
apiBlobPairToBlobPair _ = Prelude.error "Expected BlobPair to have either 'before' and/or 'after'."
