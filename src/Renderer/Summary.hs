module Renderer.Summary where

import Category
import Prologue
import Renderer
import Data.Aeson
import Data.ByteString.Builder
import Data.Record
import Data.Text (pack)
import DiffSummary
import Range

summary :: (HasField fields Category, HasField fields Range) => Renderer (Record fields)
summary diff _ = toS . toLazyByteString . fromEncoding . foldable $ pack . show <$> diffSummary diff
