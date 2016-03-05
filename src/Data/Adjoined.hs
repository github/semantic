module Data.Adjoined where

import Control.Monad

data Adjoined a = Separated a (Adjoined a) | Adjoined a | Empty
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
  return = Adjoined
  Empty >>= _ = Empty
  Adjoined a >>= f = f a
  Separated a b >>= f = f a >>= (`Separated` (b >>= f))

instance PartialSemigroup a => Monoid (Adjoined a) where
  mempty = Empty
  mappend a b = case (a, b) of
    (_, Empty) -> a
    (Empty, _) -> b
    (Adjoined a, Adjoined b) -> maybe (Separated a (Adjoined b)) Adjoined $ coalesce a b
    (Adjoined a, Separated b c) -> maybe (Separated a (Separated b c)) (`Separated` c) $ coalesce a b
    (Separated a b, c) -> Separated a (b `mappend` c)

instance PartialSemigroup Bool where
  coalesce True = Just
  coalesce False = const Nothing
