{-# LANGUAGE DeriveTraversable #-}
module Semantic.Data.Stack
( Stack(..)
) where

data Stack a
  = Nil
  | Stack a :> a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixl 4 :>

instance Semigroup (Stack a) where
  xs <> Nil       = xs
  xs <> (ys :> y) = (xs <> ys) :> y

instance Monoid (Stack a) where
  mempty = Nil
