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
  -- * Vectors
, Nat(..)
, N0
, N1
, N2
, N3
, Vec(..)
, toList
) where

import Control.Monad (guard)
import Data.Kind (Type)
import Data.Typeable
import Unsafe.Coerce (unsafeCoerce)

-- Syntax

-- | (Currently) untyped term representations.
data Term (sig :: Nat -> Type) where
  (:$:) :: KnownNat n => sig n -> Vec n (Term sig) -> Term sig

instance (forall n . Eq (sig n)) => Eq (Term sig) where
  a :$: as == b :$: bs = case sameNat a b of
    Just Refl -> a == b && as == bs
    _         -> False


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

toList :: Vec n a -> [a]
toList = \case
  Nil -> []
  Cons a as -> a : toList as
