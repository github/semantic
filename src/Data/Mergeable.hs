{-# LANGUAGE DefaultSignatures #-}
module Data.Mergeable where

import Data.Mergeable.Generic
import GHC.Generics
import Prologue

-- Classes

class Functor t => Mergeable t where
  sequenceAlt :: Alternative f => t (f a) -> f (t a)
  default sequenceAlt :: (Generic1 t, GMergeable (Rep1 t), Alternative f) => t (f a) -> f (t a)
  sequenceAlt = genericSequenceAlt


-- Instances

instance Mergeable [] where sequenceAlt = gsequenceAlt
