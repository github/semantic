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

summary :: (HasField fields Category, HasField fields Range) => Renderer (Record fields)
summary blobs diff = SummaryOutput $ Map.fromList [
    ("changes", changes),
    ("errors", errors)
  ]
  where
    changes = if null changes' then [] else [ Map.singleton summaryKey changes' ]
    errors = if null errors' then [] else [ Map.singleton summaryKey errors' ]
    (errors', changes') = partitionEithers summaries
    summaryKey = toSummaryKey (path <$> blobs)
    summaries = diffSummaries blobs diff
