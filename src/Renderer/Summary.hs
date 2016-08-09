module Renderer.Summary where

import Category
import Prologue
import Renderer
import Data.Record
import Range
import DiffSummary
import Source
import Data.Aeson

summary :: (HasField fields Category, HasField fields Range) => Renderer (Record fields)
summary blobs diff = SummaryOutput $ "summaries" .= (summaries >>= annotatedSummaries)
  where summaries = diffSummaries (source <$> blobs) diff