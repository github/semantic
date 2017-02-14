{-# LANGUAGE TupleSections, ScopedTypeVariables #-}
module Renderer.Summary where

import Prologue
import Renderer
import Data.Record
import DiffSummary
import Data.Map as Map hiding (null)
import Source hiding (null)
import Data.Aeson
import Data.List as List

summary :: (DefaultFields fields) => Renderer (Record fields)
summary blobs diff = SummaryOutput $ Map.fromList [
    ("changes", changes),
    ("errors", errors)
  ]
  where
    changes = if null changes' then mempty else Map.singleton summaryKey (toJSON <$> changes')
    errors = if null errors' then mempty else Map.singleton summaryKey (toJSON <$> errors')
    (errors', changes') = List.partition isErrorSummary summaries
    summaryKey = toSummaryKey (path <$> blobs)
    summaries = diffSummaries blobs diff
