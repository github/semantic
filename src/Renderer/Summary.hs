{-# LANGUAGE TupleSections, ScopedTypeVariables #-}
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
import Data.These
import Data.Aeson
import Data.List as List

summary :: (HasField fields Category, HasField fields Range, HasField fields SourceSpan) => Renderer (Record fields)
summary blobs diff = SummaryOutput $ Map.fromList [
    ("changes", changes),
    ("errors", errors)
  ]
  where
    changes = if null changes' then mempty else Map.singleton summaryKey (toJSON <$> changes')
    errors = if null errors' then mempty else Map.singleton summaryKey (toJSON <$> errors')
    (errors' :: [JSONSummary Text (These SourceSpan SourceSpan)], changes' :: [JSONSummary Text (These SourceSpan SourceSpan)]) = List.partition isErrorSummary summaries
    summaryKey = toSummaryKey (path <$> blobs)
    summaries = diffSummaries blobs diff
