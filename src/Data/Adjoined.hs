{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Data.Adjoined where

newtype Adjoined a = Adjoined { unAdjoined :: Maybe a }
  deriving (Eq, Foldable, Functor, Show, Traversable)

class PartialSemigroup a where
  coalesce :: a -> a -> Maybe a

instance Monoid a => PartialSemigroup a where
  coalesce = (Just .) . mappend

instance Applicative Adjoined where
  pure = Adjoined . pure
  Adjoined f <*> Adjoined a = Adjoined $ f <*> a
