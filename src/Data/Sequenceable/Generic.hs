module Data.Sequenceable.Generic where

import GHC.Generics
import Prologue

-- Classes

class GSequenceable t where
  gsequenceAlt :: Alternative f => t (f a) -> f (t a)


-- Instances

instance GSequenceable U1 where
  gsequenceAlt _ = pure U1
