module Data.Adjoined where

import Control.Monad

newtype Adjoined a = Adjoined { unAdjoined :: Maybe a }
  deriving (Eq, Foldable, Functor, Show, Traversable)

class PartialSemigroup a where
  coalesce :: a -> a -> Maybe a

instance Applicative Adjoined where
  pure = Adjoined . pure
  Adjoined f <*> Adjoined a = Adjoined $ f <*> a

instance Monad Adjoined where
  return = pure
  Adjoined a >>= f = Adjoined $ a >>= unAdjoined . f

instance PartialSemigroup a => Monoid (Adjoined a) where
  mempty = Adjoined Nothing
  mappend (Adjoined a) (Adjoined b) = Adjoined . join $ coalesce <$> a <*> b

instance PartialSemigroup Bool where
  coalesce True = Just
  coalesce False = const Nothing
