{-# LANGUAGE TupleSections #-}
module Renderer.Summary where

import Category
import Prologue
import Renderer
import Data.Record
import Range
import DiffSummary
import Data.Map as Map hiding (null)
import Source
import SourceSpan

summary :: (HasField fields Category, HasField fields Range, HasField fields SourceSpan) => Renderer (Record fields)
summary blobs diff = SummaryOutput $ Map.fromList [
    ("changes", changes),
    ("errors", errors)
  ]
  where
    changes = if null changes' then mempty else Map.singleton summaryKey changes'
    errors = if null errors' then mempty else Map.singleton summaryKey errors'
    (errors', changes') = partitionEithers summaries
    summaryKey = toSummaryKey (path <$> blobs)
    summaries = diffSummaries blobs diff
