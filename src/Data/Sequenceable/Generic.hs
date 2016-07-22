{-# LANGUAGE TypeOperators #-}
module Data.Sequenceable.Generic where

import GHC.Generics
import Prologue

-- Classes

class GSequenceable t where
  gsequenceAlt :: Alternative f => t (f a) -> f (t a)

genericSequenceAlt :: (Generic1 t, GSequenceable (Rep1 t), Alternative f) => t (f a) -> f (t a)
genericSequenceAlt = fmap to1 . gsequenceAlt . from1


-- Instances

instance GSequenceable U1 where
  gsequenceAlt _ = pure U1

instance GSequenceable Par1 where
  gsequenceAlt (Par1 a) = Par1 <$> a

instance GSequenceable (K1 i c) where
  gsequenceAlt (K1 a) = pure (K1 a)

instance GSequenceable f => GSequenceable (Rec1 f) where
  gsequenceAlt (Rec1 a) = Rec1 <$> gsequenceAlt a

instance GSequenceable f => GSequenceable (M1 i c f) where
  gsequenceAlt (M1 a) = M1 <$> gsequenceAlt a

instance (GSequenceable f, GSequenceable g) => GSequenceable (f :+: g) where
  gsequenceAlt (L1 a) = L1 <$> gsequenceAlt a
  gsequenceAlt (R1 b) = R1 <$> gsequenceAlt b

instance (GSequenceable f, GSequenceable g) => GSequenceable (f :*: g) where
  gsequenceAlt (a :*: b) = (:*:) <$> gsequenceAlt a <*> gsequenceAlt b

instance GSequenceable [] where
  gsequenceAlt (x:xs) = ((:) <$> x <|> pure identity) <*> gsequenceAlt xs
  gsequenceAlt [] = pure []
