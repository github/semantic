module Diff where

import Category
import Control.Monad.Free
import Data.Functor.Both
import Data.Set
import Patch
import Range
import Syntax
import Term

-- | An annotated syntax in a diff tree.
data Annotated a annotation f = Annotated { annotation :: !annotation, syntax :: !(Syntax a f) }
  deriving (Functor, Eq, Show, Foldable)

-- | An annotation for a source file, including the source range and semantic
-- | categories.
data Info = Info { characterRange :: !Range, categories :: !(Set Category) }
  deriving (Eq, Show)

setCharacterRange :: Info -> Range -> Info
setCharacterRange info range = Info range (Diff.categories info)

instance Categorizable Info where
  categories = Diff.categories

-- | An annotated series of patches of terms.
type Diff a annotation = Free (Annotated a (Both annotation)) (Patch (Term a annotation))

-- | Sum the result of a transform applied to all the patches in the diff.
diffSum :: (Patch (Term a annotation) -> Integer) -> Diff a annotation -> Integer
diffSum patchCost diff = sum $ fmap patchCost diff

-- | The total cost of the diff.
-- | This is the number of all leaves in all terms in all patches of the diff.
diffCost :: Diff a annotation -> Integer
diffCost = diffSum $ patchSum termSize
