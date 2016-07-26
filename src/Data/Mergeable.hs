{-# LANGUAGE DefaultSignatures #-}
module Data.Mergeable where

import Data.Functor.Identity
import Data.Mergeable.Generic
import GHC.Generics
import Prologue

-- Classes

-- | A 'Mergeable' functor is one which supports pushing itself through an 'Alternative' functor. Note the similarities with 'Traversable' & 'Crosswalk'.
--
-- This is a kind of distributive law which produces (at least) the union of the two functors’ shapes; i.e. unlike 'Traversable', an 'empty' value in the inner functor does not produce an 'empty' result, and unlike 'Crosswalk', an 'empty' value in the outer functor does not produce an 'empty' result.
class Functor t => Mergeable t where
  -- | Merge a functor by mapping its elements into an 'Alternative' functor, combining them, and pushing the 'Mergeable' functor inside.
  merge :: Alternative f => (a -> f b) -> t a -> f (t b)
  default merge :: (Generic1 t, GMergeable (Rep1 t), Alternative f) => (a -> f b) -> t a -> f (t b)
  merge = genericMerge

  -- | Sequnce a 'Mergeable' functor by 'merge'ing the 'Alternative' values.
  sequenceAlt :: Alternative f => t (f a) -> f (t a)
  sequenceAlt = merge identity


-- Instances

instance Mergeable [] where merge = gmerge

instance Mergeable Maybe

instance Mergeable Identity where merge f = fmap Identity . f . runIdentity
