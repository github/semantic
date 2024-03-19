{-# LANGUAGE GADTs #-}
module Analysis.Syntax
( -- * Syntax
  Term(..)
, subterms
  -- * Vectors
, Z
, S
, N0
, N1
, N2
, N3
, Vec(..)
) where

import           Data.Foldable (toList)
import           Data.Functor.Classes
import qualified Data.Set as Set

-- Syntax

-- | (Currently) untyped term representations.
data Term sig v where
  Var  :: v -> Term sig v
  (:$:) :: sig n -> Vec n (Term sig v) -> Term sig v

instance (Eq1 sig, Eq v) => Eq (Term sig v) where
  Var v1   == Var v2   = v1 == v2
  a :$: as == b :$: bs = liftEq (\ _ _ -> True) a b && toList as == toList bs
  _        == _        = False

instance (Ord1 sig, Ord v) => Ord (Term sig v) where
  compare (Var v1)   (Var v2)   = compare v1 v2
  compare (Var _)    _          = LT
  compare (a :$: as) (b :$: bs) = liftCompare (\ _ _ -> EQ) a b <> compare (toList as) (toList bs)
  compare _          _          = GT


subterms :: (Ord1 sig, Ord v) => Term sig v -> Set.Set (Term sig v)
subterms t = case t of
  Var _    -> Set.singleton t
  _ :$: ts -> Set.insert t (foldMap subterms ts)


-- Vectors

data Z
data S n

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2


-- FIXME: move this into its own module, or use a dependency, or something.
data Vec n a where
  Nil :: Vec Z a
  Cons :: a -> Vec n a -> Vec (S n) a

instance Eq a => Eq (Vec n a) where
  Nil       == Nil       = True
  Cons a as == Cons b bs = a == b && as == bs

instance Ord a => Ord (Vec n a) where
  compare Nil         Nil         = EQ
  compare (Cons a as) (Cons b bs) = a `compare` b <> as `compare` bs

instance Foldable (Vec n) where
  foldMap _ Nil         = mempty
  foldMap f (Cons a as) = f a <> foldMap f as
