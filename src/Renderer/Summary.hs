module Renderer.Summary where

import Prologue
import Renderer
import DiffSummary
import Data.Aeson
import Data.Text (pack)
import Text.PrettyPrint.Leijen.Text (pretty)

summary :: Renderer
summary diff _ = toS . encode $ pack . show . pretty <$> diffSummary diff
