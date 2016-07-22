module Data.Sequenceable.Generic where

import GHC.Generics
import Prologue

-- Classes

class GSequenceable t where
  gsequenceAlt :: Alternative f => t (f a) -> f (t a)


-- Instances

instance GSequenceable U1 where
  gsequenceAlt _ = pure U1

instance GSequenceable Par1 where
  gsequenceAlt (Par1 a) = Par1 <$> a

instance GSequenceable (K1 i c) where
  gsequenceAlt (K1 a) = pure (K1 a)
