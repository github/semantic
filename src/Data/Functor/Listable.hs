module Data.Functor.Listable
( Listable(..)
, cons0
, cons1
, cons2
, cons3
, cons4
, cons5
, cons6
, (\/)
, Listable1(..)
, tiers1
, Listable2(..)
, tiers2
, liftCons1
, liftCons2
) where

import Prologue
import Test.LeanCheck

class Listable1 l where
  liftTiers :: [[a]] -> [[l a]]

tiers1 :: (Listable a, Listable1 l) => [[l a]]
tiers1 = liftTiers tiers


class Listable2 l where
  liftTiers2 :: [[a]] -> [[b]] -> [[l a b]]

tiers2 :: (Listable a, Listable b, Listable2 l) => [[l a b]]
tiers2 = liftTiers2 tiers tiers


liftCons1 :: [[a]] -> (a -> b) -> [[b]]
liftCons1 tiers f = mapT f tiers `addWeight` 1

liftCons2 :: [[a]] -> [[b]] -> (a -> b -> c) -> [[c]]
liftCons2 tiers1 tiers2 f = mapT (uncurry f) (productWith (,) tiers1 tiers2) `addWeight` 1


-- Instances

instance Listable1 Maybe where
  liftTiers tiers = cons0 Nothing \/ liftCons1 tiers Just
