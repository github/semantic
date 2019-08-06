{-# LANGUAGE FlexibleInstances, QuantifiedConstraints, MultiParamTypeClasses, TypeOperators #-}
module Control.Monad.Module
( RightModule(..)
, (>=>*)
, (<=<*)
, joinr
) where

import Control.Effect.Carrier

-- | Modules over monads allow lifting of a monad’s product (i.e. 'Control.Monad.join') into another structure composed with the monad. A right-module @f m@ over a monad @m@ therefore allows one to extend @m@’s '>>=' operation to values of @f m@ using the '>>=*' operator.
--
--   Note that we are calling this a right-module following Maciej Piróg, Nicolas Wu, & Jeremy Gibbons in _Modules Over Monads and their Algebras_; confusingly, other sources refer to this as a left-module.
--
--   Laws:
--
--   Right-identity:
--
-- @
-- m >>=* return = m
-- @
--
--   Associativity:
--
-- @
-- m >>=* (k >=> h) = (m >>=* k) >>=* h
-- @
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
