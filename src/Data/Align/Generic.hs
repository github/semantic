{-# LANGUAGE DefaultSignatures, TypeOperators #-}
module Data.Align.Generic where

import Control.Monad
import Data.Align
import Data.These
import GHC.Generics
import Prologue
import Syntax

-- | Functors which can be aligned (structure-unioning-ly zipped). The default implementation will operate generically over the constructors in the aligning type.
class Functor f => GAlign f where
  galign :: f a -> f b -> Maybe (f (These a b))
  default galign :: (Generic1 f, GAlign (Rep1 f)) => f a -> f b -> Maybe (f (These a b))
  galign a b = to1 <$> galign (from1 a) (from1 b)


-- Generically-derived instances

instance Eq a => GAlign (Syntax a)


-- 'Data.Align.Align' instances

instance GAlign [] where galign = galignAlign
instance GAlign Maybe where galign = galignAlign

-- | Implements a function suitable for use as the definition of 'galign' for 'Align'able functors.
galignAlign :: Align f => f a -> f b -> Maybe (f (These a b))
galignAlign a = Just . align a


-- Generics

-- | 'GAlign' over unit constructors.
instance GAlign U1 where
  galign _ _ = Just U1

-- | 'GAlign' over parameters.
instance GAlign Par1 where
  galign (Par1 a) (Par1 b) = Just (Par1 (These a b))

-- | 'GAlign' over non-parameter fields. Only equal values are aligned.
instance Eq c => GAlign (K1 i c) where
  galign (K1 a) (K1 b) = guard (a == b) >> Just (K1 b)

-- | 'GAlign' over applications over parameters.
instance GAlign f => GAlign (Rec1 f) where
  galign (Rec1 a) (Rec1 b) = Rec1 <$> galign a b

-- | 'GAlign' over metainformation (constructor names, etc).
instance GAlign f => GAlign (M1 i c f) where
  galign (M1 a) (M1 b) = M1 <$> galign a b

-- | 'GAlign' over sums. Returns 'Nothing' for disjoint constructors.
instance (GAlign f, GAlign g) => GAlign (f :+: g) where
  galign a b = case (a, b) of
    (L1 a, L1 b) -> L1 <$> galign a b
    (R1 a, R1 b) -> R1 <$> galign a b
    _ -> Nothing

-- | 'GAlign' over products.
instance (GAlign f, GAlign g) => GAlign (f :*: g) where
  galign (a1 :*: b1) (a2 :*: b2) = (:*:) <$> galign a1 a2 <*> galign b1 b2

-- | 'GAlign' over type compositions.
instance (Traversable f, Applicative f, GAlign g) => GAlign (f :.: g) where
  galign (Comp1 a) (Comp1 b) = Comp1 <$> sequenceA (galign <$> a <*> b)
