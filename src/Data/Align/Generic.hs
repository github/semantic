{-# LANGUAGE DefaultSignatures, TypeOperators #-}
module Data.Align.Generic where

import Data.Align
import Data.These
import GHC.Generics
import Data.OrderedMap
import Prologue
import Syntax

-- Generics

class Functor f => GAlign f where
  galign :: f a -> f b -> Maybe (f (These a b))
  default galign :: (Generic1 f, GAlign (Rep1 f)) => f a -> f b -> Maybe (f (These a b))
  galign a b = to1 <$> galign (from1 a) (from1 b)

instance GAlign U1 where
  galign _ _ = Just U1

instance GAlign Par1 where
  galign (Par1 a) (Par1 b) = Just (Par1 (These a b))

instance GAlign (K1 i c) where
  galign (K1 _) (K1 b) = Just (K1 b)

instance GAlign f => GAlign (Rec1 f) where
  galign (Rec1 a) (Rec1 b) = Rec1 <$> galign a b

instance GAlign f => GAlign (M1 i c f) where
  galign (M1 a) (M1 b) = M1 <$> galign a b

instance (GAlign f, GAlign g) => GAlign (f :+: g) where
  galign a b = case (a, b) of
    (L1 a, L1 b) -> L1 <$> galign a b
    (R1 a, R1 b) -> R1 <$> galign a b
    _ -> Nothing

instance (GAlign f, GAlign g) => GAlign (f :*: g) where
  galign (a1 :*: b1) (a2 :*: b2) = (:*:) <$> galign a1 a2 <*> galign b1 b2

instance (Traversable f, Applicative f, GAlign g) => GAlign (f :.: g) where
  galign (Comp1 a) (Comp1 b) = Comp1 <$> sequenceA (galign <$> a <*> b)

instance GAlign ((,) a) where
  galign (_, a) (k, b) = Just (k, These a b)

instance GAlign [] where galign a = Just . align a
instance Eq key => GAlign (OrderedMap key) where galign a = Just . align a
instance GAlign (Syntax a)
