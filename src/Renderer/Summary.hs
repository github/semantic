module Renderer.Summary where

import Category
import Prologue
import Renderer
import Data.Aeson
import Data.Record
import Range
import SourceSpan
import DiffSummary

summary :: (HasField fields Category, HasField fields Range, HasField fields SourceSpan) => Renderer (Record fields)
summary blobs diff = toS . encode $ summaries >>= annotatedSummaries
  where summaries = diffSummary blobs diff