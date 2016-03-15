module Data.Coalescent where

import Control.Applicative
import Data.Functor.Identity

-- | The class of types which can optionally be coalesced together.
class Coalescent a where
  -- | Returns either Just the combined value of its inputs, or Nothing if they cannot be combined.
  coalesce :: Alternative f => a -> a -> f a

instance Coalescent a => Coalescent (Identity a) where
  a `coalesce` b = sequenceA (coalesce <$> a <*> b)

instance Coalescent a => Coalescent (Maybe a) where
  a `coalesce` b = sequenceA (coalesce <$> a <*> b)
