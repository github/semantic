{-# LANGUAGE DeriveTraversable #-}
module Analysis.Data.Snoc
( Snoc(..)
) where

data Snoc a = Nil | Snoc a :> a
  deriving (Foldable, Functor, Traversable)
