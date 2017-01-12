module Data.Text.Listable where

import Data.Functor.Listable
import Data.Text
import Prologue

newtype ListableText = ListableText { unListableText :: Text }

instance Listable ListableText where
  tiers = cons1 (ListableText . pack)
