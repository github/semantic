{-# LANGUAGE DeriveTraversable, LambdaCase, RankNTypes, QuantifiedConstraints, StandaloneDeriving #-}
module Data.Scope
( Incr(..)
, incr
, Scope(..)
, foldScope
, bind
, instantiate
) where

import Control.Applicative (liftA2)
import Control.Monad.Trans.Class
import Control.Monad ((>=>))
import Data.Function (on)

data Incr a b
  = Z a
  | S b
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative (Incr a) where
  pure = S
  Z a <*> _ = Z a
  S f <*> a = f <$> a

instance Monad (Incr a) where
  Z a >>= _ = Z a
  S a >>= f = f a

match :: Applicative f => (b -> Maybe a) -> b -> Incr a (f b)
match f x | Just y <- f x = Z y
          | otherwise     = S (pure x)

fromIncr :: (a -> b) -> Incr a b -> b
fromIncr f = incr f id

incr :: (a -> c) -> (b -> c) -> Incr a b -> c
incr z s = \case { Z a -> z a ; S b -> s b }


newtype Scope a f b = Scope { unScope :: f (Incr a (f b)) }
  deriving (Foldable, Functor, Traversable)

instance (Eq   a, Eq   b, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Scope a f b) where
  (==) = (==) `on` (unScope >=> sequenceA)

instance (Ord  a, Ord  b, forall a . Eq   a => Eq   (f a)
                        , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Scope a f b) where
  compare = compare `on` (unScope >=> sequenceA)

deriving instance (Show a, Show b, forall a . Show a => Show (f a)) => Show (Scope a f b)

instance Applicative f => Applicative (Scope a f) where
  pure = Scope . pure . S . pure
  Scope f <*> Scope a = Scope (liftA2 (liftA2 (<*>)) f a)

instance Monad f => Monad (Scope a f) where
  Scope e >>= f = Scope (e >>= incr (pure . Z) (>>= unScope . f))

instance MonadTrans (Scope a) where
  lift = Scope . pure . S

foldScope :: (forall a . Incr z (n a) -> m (Incr z (n a)))
          -> (forall x y . (x -> m y) -> f x -> n y)
          -> (a -> m b)
          -> Scope z f a
          -> Scope z n b
foldScope k go h = Scope . go (k . fmap (go h)) . unScope

-- | Bind occurrences of a variable in a term, producing a term in which the variable is bound.
bind :: Applicative f => (b -> Maybe a) -> f b -> Scope a f b
bind name = Scope . fmap (match name) -- FIXME: succ as little of the expression as possible, cf https://twitter.com/ollfredo/status/1145776391826358273 — can this even be done generically?

-- | Substitute a term for the free variable in a given term, producing a closed term.
instantiate :: Monad f => (a -> f b) -> Scope a f b -> f b
instantiate f = unScope >=> fromIncr f
