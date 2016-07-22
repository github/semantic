module Renderer.Summary where

import Category
import Prologue
import Renderer
import Data.Aeson
import Data.Record
import Range
import DiffSummary
import Data.Text (pack)
import Text.PrettyPrint.Leijen.Text (pretty)

summary :: (HasField fields Category, HasField fields Range) => Renderer (Record fields)
summary diff _ = toS . encode $ pack . show . pretty <$> diffSummary diff
