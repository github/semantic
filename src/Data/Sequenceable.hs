module Data.Sequenceable where

import Data.Sequenceable.Generic
import Prologue

-- Classes

class Functor t => Sequenceable t where
  sequenceAlt :: Alternative f => t (f a) -> f (t a)


-- Instances

instance Sequenceable [] where sequenceAlt = gsequenceAlt
