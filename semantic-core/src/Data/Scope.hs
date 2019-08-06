{-# LANGUAGE DeriveTraversable, LambdaCase, RankNTypes, QuantifiedConstraints, StandaloneDeriving #-}
module Data.Scope
( Incr(..)
, incr
, closed
, Scope(..)
, fromScope
, toScope
, abstract1
, abstract
, abstractEither
, instantiate1
, instantiate
, instantiateEither
, unprefix
, unprefixEither
) where

import Control.Applicative (liftA2)
import Control.Effect.Carrier
import Control.Monad ((>=>), guard)
import Control.Monad.Module
import Control.Monad.Trans.Class
import Data.Function (on)
import Data.Stack

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

match :: Applicative f => (b -> Either a c) -> b -> Incr a (f c)
match f x = either Z (S . pure) (f x)

matchMaybe :: (b -> Maybe a) -> (b -> Either a b)
matchMaybe f a = maybe (Right a) Left (f a)

incr :: (a -> c) -> (b -> c) -> Incr a b -> c
incr z s = \case { Z a -> z a ; S b -> s b }


closed :: Traversable f => f a -> Maybe (f b)
closed = traverse (const Nothing)


newtype Scope a f b = Scope { unScope :: f (Incr a (f b)) }
  deriving (Foldable, Functor, Traversable)

instance HFunctor (Scope a) where
  hmap f = Scope . f . fmap (fmap f) . unScope

instance (Eq   a, Eq   b, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Scope a f b) where
  (==) = (==) `on` fromScope

instance (Ord  a, Ord  b, forall a . Eq   a => Eq   (f a)
                        , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Scope a f b) where
  compare = compare `on` fromScope

deriving instance (Show a, Show b, forall a . Show a => Show (f a)) => Show (Scope a f b)

instance Applicative f => Applicative (Scope a f) where
  pure = Scope . pure . S . pure
  Scope f <*> Scope a = Scope (liftA2 (liftA2 (<*>)) f a)

instance Monad f => Monad (Scope a f) where
  Scope e >>= f = Scope (e >>= incr (pure . Z) (>>= unScope . f))

instance MonadTrans (Scope a) where
  lift = Scope . pure . S

instance RightModule (Scope a) where
  Scope m >>=* f = Scope (fmap (>>= f) <$> m)


fromScope :: Monad f => Scope a f b -> f (Incr a b)
fromScope = unScope >=> sequenceA

toScope :: Applicative f => f (Incr a b) -> Scope a f b
toScope = Scope . fmap (fmap pure)


-- | Bind occurrences of a variable in a term, producing a term in which the variable is bound.
abstract1 :: (Applicative f, Eq a) => a -> f a -> Scope () f a
abstract1 n = abstract (guard . (== n))

abstract :: Applicative f => (b -> Maybe a) -> f b -> Scope a f b
abstract f = abstractEither (matchMaybe f)

abstractEither :: Applicative f => (b -> Either a c) -> f b -> Scope a f c
abstractEither f = Scope . fmap (match f) -- FIXME: succ as little of the expression as possible, cf https://twitter.com/ollfredo/status/1145776391826358273


-- | Substitute a term for the free variable in a given term, producing a closed term.
instantiate1 :: Monad f => f b -> Scope a f b -> f b
instantiate1 t = instantiate (const t)

instantiate :: Monad f => (a -> f b) -> Scope a f b -> f b
instantiate f = instantiateEither (either f pure)

instantiateEither :: Monad f => (Either a b -> f c) -> Scope a f b -> f c
instantiateEither f = unScope >=> incr (f . Left) (>>= f . Right)


-- | Unwrap a (possibly-empty) prefix of @a@s wrapping a @t@ using a helper function.
unprefix :: (Int -> t -> Maybe (a, t)) -> t -> (Stack a, t)
unprefix from = unprefixEither (matchMaybe . from)

unprefixEither :: (Int -> t -> Either (a, t) b) -> t -> (Stack a, b)
unprefixEither from = go (0 :: Int) Nil
  where go i bs t = case from i t of
          Left (b, t) -> go (succ i) (bs :> b) t
          Right b     -> (bs, b)
