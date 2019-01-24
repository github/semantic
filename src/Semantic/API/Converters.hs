{-# LANGUAGE LambdaCase #-}
module Semantic.API.Converters
  ( spanToSpan
  , spanToLegacySpan
  , toChangeType
  ) where

import qualified Semantic.API.Types as API
import qualified Semantic.API.LegacyTypes as Legacy
import qualified Data.Span as Data
import qualified Data.Text as T

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
