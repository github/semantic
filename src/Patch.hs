module Patch (Patch(..), after, before, unPatch, patchSum) where

import Data.Bifunctor.These

-- | An operation to replace, insert, or delete an item.
data Patch a =
  Replace a a
  | Insert a
  | Delete a
  deriving (Foldable, Functor, Show, Eq)

-- | Return the item from the after side of the patch.
after :: Patch a -> Maybe a
after = maybeFirst . unPatch

-- | Return the item from the before side of the patch.
before :: Patch a -> Maybe a
before = maybeSecond . unPatch

-- | Return both sides of a patch.
unPatch :: Patch a -> These a a
unPatch (Replace a b) = These a b
unPatch (Insert b) = That b
unPatch (Delete a) = This a

-- | Calculate the cost of the patch given a function to compute the cost of a item.
patchSum :: (a -> Integer) -> Patch a -> Integer
patchSum termCost patch = maybe 0 termCost (before patch) + maybe 0 termCost (after patch)
