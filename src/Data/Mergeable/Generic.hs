{-# LANGUAGE TypeOperators #-}
module Data.Mergeable.Generic where

import GHC.Generics
import Prologue

-- Classes

class GMergeable t where
  gsequenceAlt :: Alternative f => t (f a) -> f (t a)

genericSequenceAlt :: (Generic1 t, GMergeable (Rep1 t), Alternative f) => t (f a) -> f (t a)
genericSequenceAlt = fmap to1 . gsequenceAlt . from1


-- Instances

instance GMergeable U1 where
  gsequenceAlt _ = pure U1

instance GMergeable Par1 where
  gsequenceAlt (Par1 a) = Par1 <$> a

instance GMergeable (K1 i c) where
  gsequenceAlt (K1 a) = pure (K1 a)

instance GMergeable f => GMergeable (Rec1 f) where
  gsequenceAlt (Rec1 a) = Rec1 <$> gsequenceAlt a

instance GMergeable f => GMergeable (M1 i c f) where
  gsequenceAlt (M1 a) = M1 <$> gsequenceAlt a

instance (GMergeable f, GMergeable g) => GMergeable (f :+: g) where
  gsequenceAlt (L1 a) = L1 <$> gsequenceAlt a
  gsequenceAlt (R1 b) = R1 <$> gsequenceAlt b

instance (GMergeable f, GMergeable g) => GMergeable (f :*: g) where
  gsequenceAlt (a :*: b) = (:*:) <$> gsequenceAlt a <*> gsequenceAlt b

instance GMergeable [] where
  gsequenceAlt (x:xs) = ((:) <$> x <|> pure identity) <*> gsequenceAlt xs
  gsequenceAlt [] = pure []
