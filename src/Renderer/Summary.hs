module Renderer.Summary where

import Prologue
import Renderer
import DiffSummary
import Data.Aeson
import Data.Text (pack)

summary :: Renderer
summary diff _ = toS . encode $ pack . show <$> diffSummary diff
