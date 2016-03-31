module Info where

import Category
import Data.Set
import Range

-- | An annotation for a source file, including the source range and semantic
-- | categories.
data Info = Info { characterRange :: !Range, categories :: !(Set Category) }
  deriving (Eq, Show)

-- | Return a new Info by replacing its characterRange.
setCharacterRange :: Info -> Range -> Info
setCharacterRange info range = Info range (Info.categories info)

instance Categorizable Info where
  categories = Info.categories
