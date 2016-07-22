{-# LANGUAGE DefaultSignatures #-}
module Data.Mergeable where

import Data.Mergeable.Generic
import GHC.Generics
import Prologue

-- Classes

class Functor t => Mergeable t where
  merge :: Alternative f => (a -> f b) -> t a -> f (t b)
  default merge :: (Generic1 t, GMergeable (Rep1 t), Alternative f) => (a -> f b) -> t a -> f (t b)
  merge = genericMerge

  sequenceAlt :: Alternative f => t (f a) -> f (t a)
  default sequenceAlt :: (Generic1 t, GMergeable (Rep1 t), Alternative f) => t (f a) -> f (t a)
  sequenceAlt = genericSequenceAlt


-- Instances

instance Mergeable [] where sequenceAlt = gmerge identity
