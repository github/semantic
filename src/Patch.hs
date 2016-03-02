module Patch where

import Data.Functor.Both as Both

-- | An operation to replace, insert, or delete an item.
data Patch a =
  Replace a a
  | Insert a
  | Delete a
  deriving (Functor, Show, Eq)

-- | Return the item from the after side of the patch.
after :: Patch a -> Maybe a
after = Both.snd . unPatch

-- | Return the item from the before side of the patch.
before :: Patch a -> Maybe a
before = Both.fst . unPatch

-- | Return both sides of a patch.
unPatch :: Patch a -> Both (Maybe a)
unPatch (Replace a b) = both (Just a) (Just b)
unPatch (Insert b) = both Nothing (Just b)
unPatch (Delete a) = both (Just a) Nothing

-- | Calculate the cost of the patch given a function to compute the cost of a item.
patchSum :: (a -> Integer) -> Patch a -> Integer
patchSum termCost patch = maybe 0 termCost (before patch) + maybe 0 termCost (after patch)
