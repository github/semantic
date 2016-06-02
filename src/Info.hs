module Info where

import Prologue
import Category
import Range

-- | An annotation for a source file, including the source range and semantic
-- | categories.
data Info = Info { characterRange :: !Range, category :: !Category, size :: !Integer }
  deriving (Eq, Show)
