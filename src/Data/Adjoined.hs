module Data.Adjoined where

import Control.Monad

data Adjoined a = Separated (Adjoined a) (Adjoined a) | Adjoined (Maybe a)
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
  return = Adjoined . Just
  Adjoined Nothing >>= _ = Adjoined Nothing
  Adjoined (Just a) >>= f = f a
  Separated a b >>= f = Separated (a >>= f) (b >>= f)

instance PartialSemigroup a => Monoid (Adjoined a) where
  mempty = Adjoined Nothing
  mappend a b = case (a, b) of
    (_, Adjoined Nothing) -> a
    (Adjoined Nothing, _) -> b
    (Adjoined (Just a'), Adjoined (Just b')) -> maybe (Separated a b) (Adjoined . Just) $ coalesce a' b'
    (a, Separated b c) -> Separated (a `mappend` b) c
    (Separated a b, c) -> Separated a (b `mappend` c)

instance PartialSemigroup Bool where
  coalesce True = Just
  coalesce False = const Nothing
