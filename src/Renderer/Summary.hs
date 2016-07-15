module Renderer.Summary where

import Prologue
import Renderer
import DiffSummary
import Data.Aeson

summary :: Renderer
summary diff _ = toS . encode $ annotatedTexts
  where summaries = diffSummary diff
        annotatedTexts :: [Text]
        annotatedTexts = join $ annotatedSummaries <$> summaries
