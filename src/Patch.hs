module Patch
( Patch(..)
, after
, before
, unPatch
, patchSum
, maybeFst
, maybeSnd
) where

import Data.These
import Prologue

-- | An operation to replace, insert, or delete an item.
data Patch a =
  Replace a a
  | Insert a
  | Delete a
  deriving (Eq, Foldable, Functor, Show, Traversable)

-- | Return the item from the after side of the patch.
after :: Patch a -> Maybe a
after = maybeSnd . unPatch

-- | Return the item from the before side of the patch.
before :: Patch a -> Maybe a
before = maybeFst . unPatch

-- | Return both sides of a patch.
unPatch :: Patch a -> These a a
unPatch (Replace a b) = These a b
unPatch (Insert b) = That b
unPatch (Delete a) = This a

-- | Calculate the cost of the patch given a function to compute the cost of a item.
patchSum :: (a -> Integer) -> Patch a -> Integer
patchSum termCost patch = maybe 0 termCost (before patch) + maybe 0 termCost (after patch)

-- | Return Just the value in This, or the first value in These, if any.
maybeFst :: These a b -> Maybe a
maybeFst = these Just (const Nothing) ((Just .) . const)

-- | Return Just the value in That, or the second value in These, if any.
maybeSnd :: These a b -> Maybe b
maybeSnd = these (const Nothing) Just ((Just .) . flip const)
