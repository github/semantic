module Data.Abstract.Live where

import Data.Abstract.Address
import Data.Set as Set

newtype Live l v = Live { unLive :: Set (Address l v) }

instance Ord l => Functor (Live l) where
  fmap f (Live as) = Live (Set.map (fmap f) as)
