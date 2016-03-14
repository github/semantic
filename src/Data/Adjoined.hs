{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Adjoined where

import Control.Monad
import Data.Coalescent
import Data.Monoid
import Data.Sequence as Seq

-- | A collection of elements which can be adjoined onto other such collections associatively.
newtype Adjoined a = Adjoined { unAdjoined :: Seq a }
  deriving (Eq, Foldable, Functor, Show, Traversable)

-- | Construct an Adjoined from a list.
fromList :: [a] -> Adjoined a
fromList = Adjoined . Seq.fromList

cons :: a -> Adjoined a -> Adjoined a
cons a (Adjoined as) = Adjoined (a <| as)

-- | Destructure a non-empty Adjoined into Just the leftmost element and the rightward remainder of the Adjoined, or Nothing otherwise.
uncons :: Adjoined a -> Maybe (a, Adjoined a)
uncons (Adjoined v) | a :< as <- viewl v = Just (a, Adjoined as)
                    | otherwise = Nothing

-- | Destructure a non-empty Adjoined into Just the rightmost element and the leftward remainder of the Adjoined, or Nothing otherwise.
unsnoc :: Adjoined a -> Maybe (Adjoined a, a)
unsnoc (Adjoined v) | as :> a <- viewr v = Just (Adjoined as, a)
                    | otherwise = Nothing

instance Applicative Adjoined where
  pure = return
  (<*>) = ap

instance Monad Adjoined where
  return = Adjoined . return
  Adjoined a >>= f = case viewl a of
    EmptyL -> Adjoined Seq.empty
    (a :< as) -> Adjoined $ unAdjoined (f a) >< unAdjoined (Adjoined as >>= f)

instance Coalescent a => Monoid (Adjoined a) where
  mempty = Adjoined mempty
  a `mappend` b | Just (as, a') <- unsnoc a,
                  Just (b', bs) <- uncons b,
                  Just coalesced <- coalesce a' b' = as <> pure coalesced <> bs
                | otherwise = Adjoined (unAdjoined a >< unAdjoined b)
