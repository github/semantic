module Semantic.API.Converters
  ( spanToSpan
  , spanToLegacySpan
  ) where

import qualified Semantic.API.Types as API
import qualified Semantic.API.LegacyTypes as Legacy
import qualified Data.Span as Data

spanToSpan :: Data.Span -> Maybe API.Span
spanToSpan Data.Span{..} = Just $ API.Span (toPos spanStart) (toPos spanEnd)
  where toPos Data.Pos{..} = Just $ API.Position posLine posColumn

spanToLegacySpan :: Data.Span -> Maybe Legacy.Span
spanToLegacySpan Data.Span{..} = Just $ Legacy.Span (toPos spanStart) (toPos spanEnd)
  where toPos Data.Pos{..} = Just $ Legacy.Position posLine posColumn
