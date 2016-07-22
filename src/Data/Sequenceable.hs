{-# LANGUAGE DefaultSignatures #-}
module Data.Sequenceable where

import Data.Sequenceable.Generic
import GHC.Generics
import Prologue

-- Classes

class Functor t => Sequenceable t where
  sequenceAlt :: Alternative f => t (f a) -> f (t a)
  default sequenceAlt :: (Generic1 t, GSequenceable (Rep1 t), Alternative f) => t (f a) -> f (t a)
  sequenceAlt = genericSequenceAlt


-- Instances

instance Sequenceable [] where sequenceAlt = gsequenceAlt
