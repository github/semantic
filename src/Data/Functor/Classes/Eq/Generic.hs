{-# LANGUAGE TypeOperators #-}
module Data.Functor.Classes.Eq.Generic
( Eq1(..)
, genericLiftEq
, gliftEq
) where

import Control.Comonad.Cofree as Cofree
import Data.Functor.Classes
import GHC.Generics

-- | Generically-derivable lifting of the 'Eq' class to unary type constructors.
class GEq1 f where
  -- | Lift an equality test through the type constructor.
  --
  --   The function will usually be applied to an equality function, but the more general type ensures that the implementation uses it to compare elements of the first container with elements of the second.
  gliftEq :: (a -> b -> Bool) -> f a -> f b -> Bool

-- | A suitable implementation of Eq1’s liftEq for Generic1 types.
genericLiftEq :: (Generic1 f, GEq1 (Rep1 f)) => (a -> b -> Bool) -> f a -> f b -> Bool
genericLiftEq f a b = gliftEq f (from1 a) (from1 b)


-- Eq1 instances

instance GEq1 [] where gliftEq = liftEq
instance GEq1 Maybe where gliftEq = liftEq
instance Eq a => GEq1 ((,) a) where gliftEq = liftEq
instance Eq a => GEq1 (Either a) where gliftEq = liftEq

instance Eq1 f => GEq1 (Cofree f) where
  gliftEq eq = go
    where go (a1 Cofree.:< f1) (a2 Cofree.:< f2) = eq a1 a2 && liftEq (gliftEq eq) f1 f2


-- Generics

instance GEq1 U1 where
  gliftEq _ _ _ = True

instance GEq1 Par1 where
  gliftEq f (Par1 a) (Par1 b) = f a b

instance Eq c => GEq1 (K1 i c) where
  gliftEq _ (K1 a) (K1 b) = a == b

instance Eq1 f => GEq1 (Rec1 f) where
  gliftEq f (Rec1 a) (Rec1 b) = liftEq f a b

instance GEq1 f => GEq1 (M1 i c f) where
  gliftEq f (M1 a) (M1 b) = gliftEq f a b

instance (GEq1 f, GEq1 g) => GEq1 (f :+: g) where
  gliftEq f a b = case (a, b) of
    (L1 a, L1 b) -> gliftEq f a b
    (R1 a, R1 b) -> gliftEq f a b
    _ -> False

instance (GEq1 f, GEq1 g) => GEq1 (f :*: g) where
  gliftEq f (a1 :*: b1) (a2 :*: b2) = gliftEq f a1 a2 && gliftEq f b1 b2

instance (Eq1 f, GEq1 g) => GEq1 (f :.: g) where
  gliftEq f (Comp1 a) (Comp1 b) = liftEq (gliftEq f) a b
