module Diff where

import Category
import Control.Monad.Free
import Data.Set
import GHC.Generics
import Patch
import Range
import Syntax
import Term

-- | An annotated syntax in a diff tree.
data Annotated a annotation f = Annotated { getAnnotation :: !annotation, getSyntax :: !(Syntax a f) }
  deriving (Foldable, Functor, Generic, Eq, Show, Traversable)

-- | An annotation for a source file, including the source range and semantic
-- | categories.
data Info = Info { characterRange :: !Range, categories :: !(Set Category) }
  deriving (Eq, Generic, Show)

instance Categorizable Info where
  categories = Diff.categories

-- | An annotated series of patches of terms.
type Diff a annotation = Free (Annotated a (annotation, annotation)) (Patch (Term a annotation))

-- | Sum the result of a transform applied to all the patches in the diff.
diffSum :: (Patch (Term a annotation) -> Integer) -> Diff a annotation -> Integer
diffSum patchCost diff = sum $ fmap patchCost diff

-- | The total cost of the diff.
-- | This is the number of all leaves in all terms in all patches of the diff.
diffCost :: Diff a annotation -> Integer
diffCost = diffSum $ patchSum termSize
