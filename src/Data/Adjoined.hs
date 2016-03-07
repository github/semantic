module Data.Adjoined where

import Control.Monad
import Data.Sequence

newtype Adjoined a = Adjoined { unAdjoined :: Seq a }
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
  Adjoined a >>= f = case viewl a of
    EmptyL -> Adjoined empty
    (a :< as) -> Adjoined $ unAdjoined (f a) >< unAdjoined (Adjoined as >>= f)

instance PartialSemigroup a => Monoid (Adjoined a) where
  mempty = Adjoined empty
  mappend = mappendBy coalesce

mappendBy :: (a -> a -> Maybe a) -> Adjoined a -> Adjoined a -> Adjoined a
mappendBy coalesce (Adjoined a) (Adjoined b) = case (viewr a, viewl b) of
  (_, EmptyL) -> Adjoined a
  (EmptyR, _) -> Adjoined b
  (as :> a', b' :< bs) -> Adjoined $ maybe (a >< b) ((as ><) . (<| bs)) (coalesce a' b')

instance PartialSemigroup Bool where
  coalesce True = Just
  coalesce False = const Nothing

instance Monoid a => PartialSemigroup (Maybe a) where
  coalesce Nothing _ = Nothing
  coalesce _ Nothing = Nothing
  coalesce (Just a) (Just b) = Just (Just (a `mappend` b))
