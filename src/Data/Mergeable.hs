{-# LANGUAGE DefaultSignatures #-}
module Data.Mergeable where

import Data.Functor.Identity
import Data.Mergeable.Generic
import GHC.Generics
import Prologue

-- Classes

class Functor t => Mergeable t where
  merge :: Alternative f => (a -> f b) -> t a -> f (t b)
  default merge :: (Generic1 t, GMergeable (Rep1 t), Alternative f) => (a -> f b) -> t a -> f (t b)
  merge = genericMerge

  sequenceAlt :: Alternative f => t (f a) -> f (t a)
  sequenceAlt = merge identity


-- Instances

instance Mergeable [] where merge = gmerge

instance Mergeable Maybe

instance Mergeable Identity where merge f = fmap Identity . f . runIdentity
