{-# LANGUAGE TypeOperators #-}
module Data.Functor.Classes.Ord.Generic
( Ord1(..)
, genericLiftCompare
) where

import Data.Functor.Classes
import Data.Semigroup
import GHC.Generics

-- | Generically-derivable lifting of the 'Ord' class to unary type constructors.
class GOrd1 f where
  -- | Lift a comparison function through the type constructor.
  --
  --   The function will usually be applied to a comparison function, but the more general type ensures that the implementation uses it to compare elements of the first container with elements of the second.
  gliftCompare :: (a -> b -> Ordering) -> f a -> f b -> Ordering

-- | A suitable implementation of Ord1’s liftCompare for Generic1 types.
genericLiftCompare :: (Generic1 f, GOrd1 (Rep1 f)) => (a -> b -> Ordering) -> f a -> f b -> Ordering
genericLiftCompare f a b = gliftCompare f (from1 a) (from1 b)


-- Generics

instance GOrd1 U1 where
  gliftCompare _ _ _ = EQ

instance GOrd1 Par1 where
  gliftCompare f (Par1 a) (Par1 b) = f a b

instance Ord c => GOrd1 (K1 i c) where
  gliftCompare _ (K1 a) (K1 b) = compare a b

instance Ord1 f => GOrd1 (Rec1 f) where
  gliftCompare f (Rec1 a) (Rec1 b) = liftCompare f a b

instance GOrd1 f => GOrd1 (M1 i c f) where
  gliftCompare f (M1 a) (M1 b) = gliftCompare f a b

instance (GOrd1 f, GOrd1 g) => GOrd1 (f :+: g) where
  gliftCompare f a b = case (a, b) of
    (L1 a, L1 b) -> gliftCompare f a b
    (R1 a, R1 b) -> gliftCompare f a b
    (L1 _, R1 _) -> LT
    (R1 _, L1 _) -> GT

instance (GOrd1 f, GOrd1 g) => GOrd1 (f :*: g) where
  gliftCompare f (a1 :*: b1) (a2 :*: b2) = gliftCompare f a1 a2 <> gliftCompare f b1 b2

instance (Ord1 f, GOrd1 g) => GOrd1 (f :.: g) where
  gliftCompare f (Comp1 a) (Comp1 b) = liftCompare (gliftCompare f) a b
