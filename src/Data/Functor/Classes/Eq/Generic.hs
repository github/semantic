module Data.Functor.Classes.Eq.Generic where

import GHC.Generics
import Prologue

class GEq1 f where
  -- | Lift an equality test through the type constructor.
  --
  --   The function will usually be applied to an equality function, but the more general type ensures that the implementation uses it to compare elements of the first container with elements of the second.
  gliftEq :: (a -> b -> Bool) -> f a -> f b -> Bool

genericLiftEq :: (Generic1 f, GEq1 (Rep1 f)) => (a -> b -> Bool) -> f a -> f b -> Bool
genericLiftEq f a b = gliftEq f (from1 a) (from1 b)


-- Generics

instance GEq1 U1 where
  gliftEq _ _ _ = True
