{-# LANGUAGE FlexibleInstances, QuantifiedConstraints, MultiParamTypeClasses, TypeOperators #-}
module Control.Monad.Module
( RightModule(..)
, (>=>*)
, (<=<*)
, joinr
) where

import Control.Effect.Carrier

class (forall g . Functor g => Functor (f g), HFunctor f) => RightModule f where
  (>>=*) :: Monad m => f m a -> (a -> m b) -> f m b
  infixl 1 >>=*

instance (RightModule f, RightModule g) => RightModule (f :+: g) where
  L l >>=* f = L (l >>=* f)
  R r >>=* f = R (r >>=* f)


(>=>*) :: (RightModule f, Monad m) => (a -> f m b) -> (b -> m c) -> (a -> f m c)
f >=>* g = \x -> f x >>=* g

infixl 1 >=>*

(<=<*) :: (RightModule f, Monad m) => (b -> m c) -> (a -> f m b) -> (a -> f m c)
g <=<* f = \x -> f x >>=* g

infixl 1 <=<*

joinr :: (RightModule f, Monad m) => f m (m a) -> f m a
joinr = (>>=* id)
