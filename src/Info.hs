module Info where

import Category
import Data.Set
import Range

-- | An annotation for a source file, including the source range and semantic
-- | categories.
data Info = Info { characterRange :: !Range, categories :: !(Set Category), size :: !Integer }
  deriving (Eq, Show)

instance Categorizable Info where
  categories = Info.categories
