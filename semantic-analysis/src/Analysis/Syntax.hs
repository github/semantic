{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Syntax
( -- * Syntax
  Term(..)
, subterms
  -- * Vectors
, Nat(..)
, N0
, N1
, N2
, N3
, Vec(..)
, toList
) where

import           Control.Monad (guard)
import           Data.Kind (Type)
import qualified Data.Set as Set
import           Data.Typeable
import           Unsafe.Coerce (unsafeCoerce)

-- Syntax

-- | (Currently) untyped term representations.
data Term (sig :: Nat -> Type) v where
  Var  :: v -> Term sig v
  (:$:) :: KnownNat n => sig n -> Vec n (Term sig v) -> Term sig v

instance (forall n . Eq (sig n), Eq v) => Eq (Term sig v) where
  Var v1   == Var v2   = v1 == v2
  a :$: as == b :$: bs = case sameNat a b of
    Just Refl -> a == b && as == bs
    _         -> False
  _        == _        = False

instance (forall n . Ord (sig n), Ord v) => Ord (Term sig v) where
  compare (Var v1)   (Var v2)   = compare v1 v2
  compare (Var _)    _          = LT
  compare (a :$: as) (b :$: bs) = case sameNat a b of
    Just Refl -> compare a b <> compare as bs
    _         -> reifyNat a `compare` reifyNat b -- lol
  compare _          _          = GT


subterms :: (forall n . Ord (sig n), Ord v) => Term sig v -> Set.Set (Term sig v)
subterms t = case t of
  Var _    -> Set.singleton t
  _ :$: ts -> Set.insert t (foldMap subterms ts)


-- Vectors

data Nat
  = Z
  | S Nat
  deriving (Eq, Ord, Show)

type N0 = 'Z
type N1 = 'S N0
type N2 = 'S N1
type N3 = 'S N2


-- | Reify 'Nat's back from type-level singletons.
class KnownNat (n :: Nat) where
  reifyNat :: proxy n -> Nat

instance KnownNat 'Z where
  reifyNat _ = Z

instance KnownNat n => KnownNat ('S n) where
  reifyNat _ = S (reifyNat (undefined :: proxy n))


-- | Test the equality of type-level 'Nat's at runtime, generating a type-level equality if equal.
sameNat :: forall a b proxy1 proxy2 . (KnownNat a, KnownNat b) => proxy1 a -> proxy2 b -> Maybe (a :~: b)
sameNat a b = unsafeCoerce Refl <$ guard (reifyNat a == reifyNat b)


-- FIXME: move this into its own module, or use a dependency, or something.
data Vec (n :: Nat) a where
  Nil :: Vec 'Z a
  Cons :: a -> Vec n a -> Vec ('S n) a

instance Eq a => Eq (Vec n a) where
  Nil       == Nil       = True
  Cons a as == Cons b bs = a == b && as == bs

instance Ord a => Ord (Vec n a) where
  compare Nil         Nil         = EQ
  compare (Cons a as) (Cons b bs) = a `compare` b <> as `compare` bs

instance Foldable (Vec n) where
  foldMap _ Nil         = mempty
  foldMap f (Cons a as) = f a <> foldMap f as


toList :: Vec n a -> [a]
toList = \case
  Nil -> []
  Cons a as -> a : toList as
