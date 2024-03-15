{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module Analysis.Syntax
( -- * Syntax
  Term(..)
  -- * Vectors
, Vec(..)
, toList
) where

import Data.Kind (Type)
import GHC.TypeLits (Natural, type (+))

-- Syntax

-- | (Currently) untyped term representations.
data Term (sig :: Natural -> Type) where
  (:$:) :: sig n -> Vec n (Term sig) -> Term sig


-- Vectors

-- FIXME: move this into its own module, or use a dependency, or something.
data Vec (n :: Natural) a where
  Nil :: Vec 0 a
  Cons :: a -> Vec n a -> Vec (1 + n) a

toList :: Vec n a -> [a]
toList = \case
  Nil -> []
  Cons a as -> a : toList as
