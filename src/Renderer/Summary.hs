module Renderer.Summary where

import Renderer
import DiffSummary
import Data.Text (pack)

summary :: Renderer
summary diff sources = pack . show $ diffSummary diff
