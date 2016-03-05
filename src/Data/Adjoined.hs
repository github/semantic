module Data.Adjoined where

import Control.Monad

newtype Adjoined a = Adjoined { unAdjoined :: [a] }
  deriving (Eq, Foldable, Functor, Show, Traversable)

-- | A partial semigroup consists of a set and a binary operation which is associative but not necessarily closed.
-- |
-- | This is one possible generalization of semigroups, alongside the better-known Magma, which has a binary operation which is closed but not necessarily associative.
class PartialSemigroup a where
  coalesce :: a -> a -> Maybe a

instance Applicative Adjoined where
  pure = return
  (<*>) = ap

instance Monad Adjoined where
  return = Adjoined . return
  Adjoined [] >>= _ = Adjoined []
  Adjoined (a : as) >>= f = Adjoined $ unAdjoined (f a) ++ unAdjoined (Adjoined as >>= f)

instance PartialSemigroup a => Monoid (Adjoined a) where
  mempty = Adjoined []
  mappend a (Adjoined []) = a
  mappend (Adjoined as) (Adjoined (b : bs)) = Adjoined (adjoinRight as)
    where adjoinRight [] = b : bs
          adjoinRight [a] = maybe (a : b : bs) (:bs) (coalesce a b)
          adjoinRight (a : as) = a : adjoinRight as

instance PartialSemigroup Bool where
  coalesce True = Just
  coalesce False = const Nothing
