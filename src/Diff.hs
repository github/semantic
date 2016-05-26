module Diff where

import Prologue
import Control.Monad.Free
import Data.Functor.Both
import Patch
import Syntax
import Term

-- | An annotated syntax in a diff tree.
data Annotated a annotation f = Annotated { annotation :: !annotation, syntax :: !(Syntax a f) }
  deriving (Functor, Eq, Show, Foldable)

-- | An annotated series of patches of terms.
type Diff a annotation = Free (Annotated a (Both annotation)) (Patch (Term a annotation))

-- | Sum the result of a transform applied to all the patches in the diff.
diffSum :: (Patch (Term a annotation) -> Integer) -> Diff a annotation -> Integer
diffSum patchCost diff = sum $ fmap patchCost diff

-- | The sum of the node count of the diff’s patches.
diffCost :: Diff a annotation -> Integer
diffCost = diffSum $ patchSum termSize
