module Data.Text.Listable where

import Data.Text

newtype ListableText = ListableText { unListableText :: Text }
