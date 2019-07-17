{-# LANGUAGE DeriveTraversable, FlexibleInstances, LambdaCase, MultiParamTypeClasses, QuantifiedConstraints, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Data.Term
( Term(..)
, Syntax(..)
, iter
) where

import Control.Effect.Carrier
import Control.Monad (ap)
import Control.Monad.Module
import Data.Scope

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


class (HFunctor sig, forall g . Functor g => Functor (sig g)) => Syntax sig where
  foldSyntax :: (forall x y . (x -> m y) -> f x -> n y)
             -> (forall a . Incr () (n a) -> m (Incr () (n a)))
             -> (a -> m b)
             -> sig f a
             -> sig n b

instance Syntax (Scope ()) where
  foldSyntax go bound free = Scope . go (bound . fmap (go free)) . unScope

instance (Syntax l, Syntax r) => Syntax (l :+: r) where
  foldSyntax go bound free (L l) = L (foldSyntax go bound free l)
  foldSyntax go bound free (R r) = R (foldSyntax go bound free r)


iter :: forall m n sig a b
     .  Syntax sig
     => (forall a . m a -> n a)
     -> (forall a . sig n a -> n a)
     -> (forall a . Incr () (n a) -> m (Incr () (n a)))
     -> (a -> m b)
     -> Term sig a
     -> n b
iter var alg bound = go
  where go :: forall x y . (x -> m y) -> Term sig x -> n y
        go free = \case
          Var a -> var (free a)
          Term t -> alg (foldSyntax go bound free t)
