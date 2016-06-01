module Renderer.Summary where

import Prologue
import Renderer
import DiffSummary
import Data.Aeson
import Data.ByteString.Builder
import Data.Text (pack)

summary :: Renderer
summary diff sources = toS . toLazyByteString . fromEncoding . foldable $ pack . show <$> diffSummary diff
