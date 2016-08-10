module Renderer.Summary where

import Category
import Prologue
import Renderer
import Data.Record
import Range
import DiffSummary
import Source
import Data.Aeson
import Data.Functor.Both (runBothWith)

summary :: (HasField fields Category, HasField fields Range) => Renderer (Record fields)
summary blobs diff = SummaryOutput $ (runBothWith toSummaryKey (path <$> blobs)) .= (summaries >>= annotatedSummaries)
  where summaries = diffSummaries (source <$> blobs) diff
        toSummaryKey before after = toS $ if before == after then after else before <> " -> " <> after