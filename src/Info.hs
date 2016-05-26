module Info where

import Prologue
import Category
import Range

-- | An annotation for a source file, including the source range and semantic
-- | categories.
data Info = Info { characterRange :: !Range, categories :: !(Set Category), size :: !Integer }
  deriving (Eq, Show)

instance Categorizable Info where
  categories = Info.categories

maybeFirstCategory :: (Categorizable a) => a -> Maybe Category
maybeFirstCategory term = listToMaybe . toList $ Category.categories term
