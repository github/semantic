{-# LANGUAGE DefaultSignatures, TypeOperators #-}
module Data.Align.Generic where

import Control.Monad
import Data.Align
import Data.These
import GHC.Generics
import Prologue

-- | Functors which can be aligned (structure-unioning-ly zipped). The default implementation will operate generically over the constructors in the aligning type.
class GAlign f where
  galign :: f a -> f b -> Maybe (f (These a b))
  galign = galignWith identity

  -- | Perform generic alignment of values of some functor, applying the given function to alignments of elements.
  galignWith :: (These a b -> c) -> f a -> f b -> Maybe (f c)
  default galignWith :: (Generic1 f, GAlign (Rep1 f)) => (These a b -> c) -> f a -> f b -> Maybe (f c)
  galignWith f a b = to1 <$> galignWith f (from1 a) (from1 b)


-- 'Data.Align.Align' instances

instance GAlign [] where galign = galignAlign
instance GAlign Maybe where galign = galignAlign
instance GAlign Identity where
  galignWith f (Identity a) (Identity b) = Just (Identity (f (These a b)))

-- | Implements a function suitable for use as the definition of 'galign' for 'Align'able functors.
galignAlign :: Align f => f a -> f b -> Maybe (f (These a b))
galignAlign a = Just . align a


-- Generics

-- | 'GAlign' over unit constructors.
instance GAlign U1 where
  galignWith _ _ _ = Just U1

-- | 'GAlign' over parameters.
instance GAlign Par1 where
  galignWith f (Par1 a) (Par1 b) = Just (Par1 (f (These a b)))

-- | 'GAlign' over non-parameter fields. Only equal values are aligned.
instance Eq c => GAlign (K1 i c) where
  galignWith _ (K1 a) (K1 b) = guard (a == b) >> Just (K1 b)

-- | 'GAlign' over applications over parameters.
instance GAlign f => GAlign (Rec1 f) where
  galignWith f (Rec1 a) (Rec1 b) = Rec1 <$> galignWith f a b

-- | 'GAlign' over metainformation (constructor names, etc).
instance GAlign f => GAlign (M1 i c f) where
  galignWith f (M1 a) (M1 b) = M1 <$> galignWith f a b

-- | 'GAlign' over sums. Returns 'Nothing' for disjoint constructors.
instance (GAlign f, GAlign g) => GAlign (f :+: g) where
  galignWith f a b = case (a, b) of
    (L1 a, L1 b) -> L1 <$> galignWith f a b
    (R1 a, R1 b) -> R1 <$> galignWith f a b
    _ -> Nothing

-- | 'GAlign' over products.
instance (GAlign f, GAlign g) => GAlign (f :*: g) where
  galignWith f (a1 :*: b1) (a2 :*: b2) = (:*:) <$> galignWith f a1 a2 <*> galignWith f b1 b2

-- | 'GAlign' over type compositions.
instance (Traversable f, Applicative f, GAlign g) => GAlign (f :.: g) where
  galignWith f (Comp1 a) (Comp1 b) = Comp1 <$> sequenceA (galignWith f <$> a <*> b)
