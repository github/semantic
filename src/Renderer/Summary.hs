module Renderer.Summary where

import Category
import Prologue hiding (isPrefixOf)
import Data.Text (isPrefixOf)
import Renderer
import Data.Record
import Range
import DiffSummary
import Data.List (partition)
import Data.Map as Map hiding (partition)
import Source

summary :: (HasField fields Category, HasField fields Range) => Renderer (Record fields)
summary blobs diff = SummaryOutput $ Map.fromList [("changes", changes), ("errors", errors)]
  where
    changes = ((toSummaryKey (path <$> blobs) <> ": ") <>) <$> changes'
    (errors, changes') = partition ("Error" `isPrefixOf`) summaries


    summaries = diffSummaries blobs diff
