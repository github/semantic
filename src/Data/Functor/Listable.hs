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
, liftCons3
, liftCons4
) where

import Data.Bifunctor.Join
import Data.These
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

liftCons3 :: [[a]] -> [[b]] -> [[c]] -> (a -> b -> c -> d) -> [[d]]
liftCons3 tiers1 tiers2 tiers3 f = mapT (uncurry3 f) (productWith (\ x (y, z) -> (x, y, z)) tiers1 (liftCons2 tiers2 tiers3 (,)) ) `addWeight` 1
  where uncurry3 f (a, b, c) = f a b c

liftCons4 :: [[a]] -> [[b]] -> [[c]] -> [[d]] -> (a -> b -> c -> d -> e) -> [[e]]
liftCons4 tiers1 tiers2 tiers3 tiers4 f = mapT (uncurry4 f) (productWith (\ x (y, z, w) -> (x, y, z, w)) tiers1 (liftCons3 tiers2 tiers3 tiers4 (,,)) ) `addWeight` 1
  where uncurry4 f (a, b, c, d) = f a b c d


-- Instances

instance Listable1 Maybe where
  liftTiers tiers = cons0 Nothing \/ liftCons1 tiers Just

instance Listable2 (,) where
  liftTiers2 = productWith (,)

instance Listable2 Either where
  liftTiers2 leftTiers rightTiers = liftCons1 leftTiers Left \/ liftCons1 rightTiers Right

instance Listable a => Listable1 ((,) a) where
  liftTiers = liftTiers2 tiers

instance Listable1 [] where
  liftTiers tiers = go
    where go = cons0 [] \/ liftCons2 tiers go (:)

instance Listable2 p => Listable1 (Join p) where
  liftTiers tiers = liftCons1 (liftTiers2 tiers tiers) Join

instance Listable2 These where
  liftTiers2 this that = liftCons1 this This \/ liftCons1 that That \/ liftCons2 this that These

instance Listable1 f => Listable2 (CofreeF f) where
  liftTiers2 annotationTiers recurTiers = liftCons2 annotationTiers (liftTiers recurTiers) (:<)

instance Listable1 f => Listable1 (Cofree f) where
  liftTiers annotationTiers = go
    where go = liftCons1 (liftTiers2 annotationTiers go) cofree

instance Listable1 f => Listable2 (FreeF f) where
  liftTiers2 pureTiers recurTiers = liftCons1 pureTiers Pure \/ liftCons1 (liftTiers recurTiers) Free

instance Listable1 f => Listable1 (Free f) where
  liftTiers pureTiers = go
    where go = liftCons1 (liftTiers2 pureTiers go) free
