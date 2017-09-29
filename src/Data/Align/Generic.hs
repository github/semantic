{-# LANGUAGE DataKinds, DefaultSignatures, TypeOperators, UndecidableInstances #-}
module Data.Align.Generic where

import Control.Applicative
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.These
import Data.Union
import GHC.Generics

-- | Functors which can be aligned (structure-unioning-ly zipped). The default implementation will operate generically over the constructors in the aligning type.
class GAlign f where
  -- | Perform generic alignment of values of some functor, applying the given function to alignments of elements.
  galignWith :: Alternative g => (These a1 a2 -> g b) -> f a1 -> f a2 -> g (f b)
  default galignWith :: (Alternative g, Generic1 f, GAlign (Rep1 f)) => (These a1 a2 -> g b) -> f a1 -> f a2 -> g (f b)
  galignWith f a b = to1 <$> galignWith f (from1 a) (from1 b)

galign :: (Alternative g, GAlign f) => f a1 -> f a2 -> g (f (These a1 a2))
galign = galignWith pure

-- 'Data.Align.Align' instances

instance GAlign Maybe where
  galignWith f (Just a1) (Just a2) = Just <$> f (These a1 a2)
  galignWith f (Just a1) Nothing   = Just <$> f (This a1)
  galignWith f Nothing   (Just a2) = Just <$> f (That a2)
  galignWith _ Nothing   Nothing   = pure Nothing

instance GAlign [] where
  galignWith f (a1:as1) (a2:as2) = (:) <$> f (These a1 a2) <*> galignWith f as1 as2
  galignWith f []       as2      = traverse (f . That) as2
  galignWith f as1      []       = traverse (f . This) as1

instance GAlign NonEmpty where
  galignWith f (a1:|as1) (a2:|as2) = (:|) <$> f (These a1 a2) <*> galignWith f as1 as2

instance Apply GAlign fs => GAlign (Union fs) where
  galignWith f = (fromMaybe empty .) . apply2' (Proxy :: Proxy GAlign) (\ inj -> (fmap inj .) . galignWith f)


-- Generics

-- | 'GAlign' over unit constructors.
instance GAlign U1 where
  galignWith _ _ _ = pure U1

-- | 'GAlign' over parameters.
instance GAlign Par1 where
  galignWith f (Par1 a) (Par1 b) = Par1 <$> f (These a b)

-- | 'GAlign' over non-parameter fields. Only equal values are aligned.
instance Eq c => GAlign (K1 i c) where
  galignWith _ (K1 a) (K1 b) = guard (a == b) *> pure (K1 b)

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
    _ -> empty

-- | 'GAlign' over products.
instance (GAlign f, GAlign g) => GAlign (f :*: g) where
  galignWith f (a1 :*: b1) (a2 :*: b2) = (:*:) <$> galignWith f a1 a2 <*> galignWith f b1 b2

-- | 'GAlign' over type compositions.
instance (Traversable f, Applicative f, GAlign g) => GAlign (f :.: g) where
  galignWith f (Comp1 a) (Comp1 b) = Comp1 <$> sequenceA (galignWith f <$> a <*> b)
