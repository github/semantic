{-# LANGUAGE TupleSections #-}
module Renderer.Summary where

import Category
import Prologue
import Renderer
import Data.Record
import Range
import DiffSummary
import Data.Map as Map
import Source

summary :: (HasField fields Category, HasField fields Range) => Renderer (Record fields)
summary blobs diff = SummaryOutput $ Map.fromList [
    ("changes", [ Map.singleton summaryKey changes ]),
    ("errors", [ Map.singleton summaryKey errors ])
  ]
  where
    summaryKey = toSummaryKey (path <$> blobs)
    (errors, changes) = partitionEithers summaries
    summaries = diffSummaries blobs diff
