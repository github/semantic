{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Functor.Listable
( Listable(..)
, mapT
, cons0
, cons1
, cons2
, cons3
, cons4
, cons5
, cons6
, (\/)
, addWeight
, ofWeight
) where

import qualified Analysis.Name as Name
import Data.Bifunctor.Join
import Data.Edit
import qualified Data.Language as Language
import Data.List.NonEmpty
import Data.Term
import Data.Text as T (Text, pack)
import Data.Sum
import Source.Loc
import Source.Span
import Test.LeanCheck

type Tier a = [a]

-- | Lifting of 'Listable' to @* -> *@.
class Listable1 l where
  -- | The tiers for @l :: * -> *@, parameterized by the tiers for @a :: *@.
  liftTiers :: [Tier a] -> [Tier (l a)]

-- | A suitable definition of 'tiers' for 'Listable1' type constructors parameterized by 'Listable' types.
tiers1 :: (Listable a, Listable1 l) => [Tier (l a)]
tiers1 = liftTiers tiers


-- | Lifting of 'Listable' to @* -> * -> *@.
class Listable2 l where
  -- | The tiers for @l :: * -> * -> *@, parameterized by the tiers for @a :: *@ & @b :: *@.
  liftTiers2 :: [Tier a] -> [Tier b] -> [Tier (l a b)]

-- | A suitable definition of 'tiers' for 'Listable2' type constructors parameterized by 'Listable' types.
tiers2 :: (Listable a, Listable b, Listable2 l) => [Tier (l a b)]
tiers2 = liftTiers2 tiers tiers


-- | Lifts a unary constructor to a list of tiers, given a list of tiers for its argument.
--
--   Commonly used in the definition of 'Listable1' and 'Listable2' instances.
liftCons1 :: [Tier a] -> (a -> b) -> [Tier b]
liftCons1 tiers f = mapT f tiers `addWeight` 1

-- | Lifts a binary constructor to a list of tiers, given lists of tiers for its arguments.
--
--   Commonly used in the definition of 'Listable1' and 'Listable2' instances.
liftCons2 :: [Tier a] -> [Tier b] -> (a -> b -> c) -> [Tier c]
liftCons2 tiers1 tiers2 f = mapT (uncurry f) (liftTiers2 tiers1 tiers2) `addWeight` 1

-- | Lifts a ternary constructor to a list of tiers, given lists of tiers for its arguments.
--
--   Commonly used in the definition of 'Listable1' and 'Listable2' instances.
liftCons3 :: [Tier a] -> [Tier b] -> [Tier c] -> (a -> b -> c -> d) -> [Tier d]
liftCons3 tiers1 tiers2 tiers3 f = mapT (uncurry3 f) (tiers1 >< tiers2 >< tiers3) `addWeight` 1
  where uncurry3 f (a, (b, c)) = f a b c

-- | Lifts a quaternary constructor to a list of tiers, given lists of tiers for its arguments.
--
--   Commonly used in the definition of 'Listable1' and 'Listable2' instances.
liftCons4 :: [Tier a] -> [Tier b] -> [Tier c] -> [Tier d] -> (a -> b -> c -> d -> e) -> [Tier e]
liftCons4 tiers1 tiers2 tiers3 tiers4 f = mapT (uncurry4 f) (tiers1 >< tiers2 >< tiers3 >< tiers4) `addWeight` 1
  where uncurry4 f (a, (b, (c, d))) = f a b c d

-- | Lifts a senary constructor to a list of tiers, given lists of tiers for its arguments.
--
--   Commonly used in the definition of 'Listable1' and 'Listable2' instances.
liftCons6 :: [Tier a] -> [Tier b] -> [Tier c] -> [Tier d] -> [Tier e] -> [Tier f] -> (a -> b -> c -> d -> e -> f -> g) -> [Tier g]
liftCons6 tiers1 tiers2 tiers3 tiers4 tiers5 tiers6 f = mapT (uncurry6 f) (tiers1 >< tiers2 >< tiers3 >< tiers4 >< tiers5 >< tiers6) `addWeight` 1
  where uncurry6 g (a, (b, (c, (d, (e, f))))) = g a b c d e f

-- Instances

instance Listable1 Maybe where
  liftTiers tiers = cons0 Nothing \/ liftCons1 tiers Just

instance Listable2 (,) where
  liftTiers2 = (><)

instance Listable2 Either where
  liftTiers2 leftTiers rightTiers = liftCons1 leftTiers Left \/ liftCons1 rightTiers Right

instance Listable a => Listable1 ((,) a) where
  liftTiers = liftTiers2 tiers

instance Listable1 [] where
  liftTiers tiers = go
    where go = cons0 [] \/ liftCons2 tiers go (:)

instance Listable1 NonEmpty where
  liftTiers tiers = liftCons2 tiers (liftTiers tiers) (:|)

instance Listable2 p => Listable1 (Join p) where
  liftTiers tiers = liftCons1 (liftTiers2 tiers tiers) Join

instance Listable1 f => Listable2 (TermF f) where
  liftTiers2 annotationTiers recurTiers = liftCons2 annotationTiers (liftTiers recurTiers) In

instance (Listable1 f, Listable a) => Listable1 (TermF f a) where
  liftTiers = liftTiers2 tiers

instance (Listable1 f, Listable a, Listable b) => Listable (TermF f a b) where
  tiers = tiers1

instance Listable1 f => Listable1 (Term f) where
  liftTiers annotationTiers = go
    where go = liftCons1 (liftTiers2 annotationTiers go) Term

instance (Listable1 f, Listable a) => Listable (Term f a) where
  tiers = tiers1

instance Listable2 Edit where
  liftTiers2 t1 t2 = liftCons1 t2 Insert \/ liftCons1 t1 Delete \/ liftCons2 t1 t2 Compare

instance (Listable a, Listable b) => Listable (Edit a b) where
  tiers = tiers2


instance (Listable1 f, Listable1 (Sum (g ': fs))) => Listable1 (Sum (f ': g ': fs)) where
  liftTiers tiers = (inject `mapT` ((liftTiers :: [Tier a] -> [Tier (f a)]) tiers)) \/ (weaken `mapT` ((liftTiers :: [Tier a] -> [Tier (Sum (g ': fs) a)]) tiers))

instance Listable1 f => Listable1 (Sum '[f]) where
  liftTiers tiers = inject `mapT` ((liftTiers :: [Tier a] -> [Tier (f a)]) tiers)

instance (Listable1 (Sum fs), Listable a) => Listable (Sum fs a) where
  tiers = tiers1


instance Listable Name.Name where
  tiers = cons1 Name.name

instance Listable Text where
  tiers = pack `mapT` tiers

instance Listable Language.Language where
  tiers
    =  cons0 Language.Go
    \/ cons0 Language.JavaScript
    \/ cons0 Language.Python
    \/ cons0 Language.Ruby
    \/ cons0 Language.TypeScript

instance Listable Loc where
  tiers = cons2 Loc

instance Listable Range where
  tiers = cons2 Range

instance Listable Pos where
  tiers = cons2 Pos

instance Listable Span where
  tiers = cons2 Span
