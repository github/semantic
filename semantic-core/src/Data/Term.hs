{-# LANGUAGE DeriveTraversable, QuantifiedConstraints, StandaloneDeriving, UndecidableInstances #-}
module Data.Term
( Term(..)
) where

import Control.Monad (ap)
import Control.Monad.Module

data Term sig a
  = Var a
  | Term (sig (Term sig) a)

deriving instance (Show a, forall g x . (Show x, forall y . Show y => Show (g y)) => Show (sig g x)) => Show (Term sig a)

deriving instance ( forall g . Foldable    g => Foldable    (sig g)) => Foldable    (Term sig)
deriving instance ( forall g . Functor     g => Functor     (sig g)) => Functor     (Term sig)
deriving instance ( forall g . Foldable    g => Foldable    (sig g)
                  , forall g . Functor     g => Functor     (sig g)
                  , forall g . Traversable g => Traversable (sig g)) => Traversable (Term sig)

instance RightModule sig => Applicative (Term sig) where
  pure = Var
  (<*>) = ap

instance RightModule sig => Monad (Term sig) where
  Var  a >>= f = f a
  Term t >>= f = Term (t >>=* f)
