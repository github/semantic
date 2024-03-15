{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
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

import Data.Kind (Type)

-- Syntax

-- | (Currently) untyped term representations.
data Term (sig :: Nat -> Type) where
  (:$:) :: sig n -> Vec n (Term sig) -> Term sig


-- Vectors

data Nat
  = Z
  | S Nat
  deriving (Eq, Ord, Show)

type N0 = 'Z
type N1 = 'S N0
type N2 = 'S N1
type N3 = 'S N2

-- FIXME: move this into its own module, or use a dependency, or something.
data Vec (n :: Nat) a where
  Nil :: Vec 'Z a
  Cons :: a -> Vec n a -> Vec ('S n) a

toList :: Vec n a -> [a]
toList = \case
  Nil -> []
  Cons a as -> a : toList as
