{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Patch
( Patch(..)
, replacing
, inserting
, deleting
, after
, before
, unPatch
, patchSum
, maybeFst
, maybeSnd
, mapPatch
) where

import Control.DeepSeq
import Data.Align
import Data.Functor.Listable
import Data.These
import GHC.Generics

-- | An operation to replace, insert, or delete an item.
data Patch a
  = Replace a a
  | Insert a
  | Delete a
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable, NFData)


-- DSL

-- | Constructs the replacement of one value by another in an Applicative context.
replacing :: Applicative f => a -> a -> f (Patch a)
replacing = (pure .) . Replace

-- | Constructs the insertion of a value in an Applicative context.
inserting :: Applicative f => a -> f (Patch a)
inserting = pure . Insert

-- | Constructs the deletion of a value in an Applicative context.
deleting :: Applicative f => a -> f (Patch a)
deleting = pure . Delete


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

mapPatch :: (a -> b) -> (a -> b) -> Patch a -> Patch b
mapPatch f _ (Delete  a  ) = Delete  (f a)
mapPatch _ g (Insert    b) = Insert  (g b)
mapPatch f g (Replace a b) = Replace (f a) (g b)

-- | Calculate the cost of the patch given a function to compute the cost of a item.
patchSum :: (a -> Int) -> Patch a -> Int
patchSum termCost patch = maybe 0 termCost (before patch) + maybe 0 termCost (after patch)

-- | Return Just the value in This, or the first value in These, if any.
maybeFst :: These a b -> Maybe a
maybeFst = these Just (const Nothing) ((Just .) . const)

-- | Return Just the value in That, or the second value in These, if any.
maybeSnd :: These a b -> Maybe b
maybeSnd = these (const Nothing) Just ((Just .) . flip const)


-- Instances

instance Listable1 Patch where
  liftTiers t = liftCons1 t Insert \/ liftCons1 t Delete \/ liftCons2 t t Replace

instance Listable a => Listable (Patch a) where
  tiers = tiers1

instance Crosswalk Patch where
  crosswalk f (Replace a b) = alignWith (these Delete Insert Replace) (f a) (f b)
  crosswalk f (Insert b) = Insert <$> f b
  crosswalk f (Delete a) = Delete <$> f a
