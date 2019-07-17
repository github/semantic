{-# LANGUAGE QuantifiedConstraints, StandaloneDeriving, UndecidableInstances #-}
module Data.Term
( Term(..)
) where

data Term sig a
  = Var a
  | Term (sig (Term sig) a)

deriving instance (Show a, forall g x . (Show x, forall y . Show y => Show (g y)) => Show (sig g x)) => Show (Term sig a)
