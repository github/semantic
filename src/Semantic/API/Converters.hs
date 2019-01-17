module Semantic.API.Converters
  ( spanToSpan
  ) where

import qualified Semantic.API.Types as API
import qualified Data.Span as Data

spanToSpan :: Data.Span -> Maybe API.Span
spanToSpan Data.Span{..} = Just $ API.Span (toPos spanStart) (toPos spanEnd)
  where toPos Data.Pos{..} = Just $ API.Position posLine posColumn
