module Renderer.Summary where

import Category
import Prologue hiding (isPrefixOf)
import Data.Text (isPrefixOf)
import Renderer
import Data.Record
import Range
import DiffSummary
import Data.Aeson
import Data.List (partition)
import Data.Functor.Both (runBothWith)
import Source

summary :: (HasField fields Category, HasField fields Range) => Renderer (Record fields)
summary blobs diff = SummaryOutput $ "changes" .= changes <> "errors" .= errors
  where
    changes = ((toSummaryKey (path <$> blobs) <> ": ") <>) <$> changes'
    (errors, changes') = partition ("Error" `isPrefixOf`) summaries
    toSummaryKey = runBothWith $ \before after ->
      toS $ if before == after then after else before <> " -> " <> after

    summaries = diffSummaries blobs diff
