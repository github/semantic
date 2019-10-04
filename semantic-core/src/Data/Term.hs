{-# LANGUAGE DeriveTraversable, FlexibleInstances, MultiParamTypeClasses, QuantifiedConstraints, RankNTypes, StandaloneDeriving, UndecidableInstances #-}
module Data.Term
( Term(..)
, hoistTerm
) where

import Control.Carrier
import Control.Monad (ap)
import Control.Monad.Module

data Term sig a
  = Var a
  | Term (sig (Term sig) a)

deriving instance ( Eq a
                  , RightModule sig
                  , forall g x . (Eq  x, Monad g, forall y . Eq  y => Eq  (g y)) => Eq  (sig g x)
                  )
               => Eq  (Term sig a)
deriving instance ( Ord a
                  , RightModule sig
                  , forall g x . (Eq  x, Monad g, forall y . Eq  y => Eq  (g y)) => Eq  (sig g x)
                  , forall g x . (Ord x, Monad g, forall y . Eq  y => Eq  (g y)
                                                , forall y . Ord y => Ord (g y)) => Ord (sig g x)
                  )
               => Ord (Term sig a)
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

instance RightModule sig => Carrier sig (Term sig) where
  eff = Term


hoistTerm :: (HFunctor sig, forall g . Functor g => Functor (sig g)) => (forall m x . sig m x -> sig' m x) -> Term sig a -> Term sig' a
hoistTerm f = go
  where go (Var v)  = Var v
        go (Term t) = Term (f (hmap (hoistTerm f) t))
