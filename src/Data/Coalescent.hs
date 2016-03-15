module Data.Coalescent where

import Control.Applicative
import Data.Functor.Identity

-- | The class of types which can optionally be coalesced together.
class Coalescent a where
  -- | Returns the result of coalescing the operands together in an Alternative context. If they cannot be coalesced, they should each be produced individually.
  coalesce :: Alternative f => a -> a -> f a

instance Coalescent a => Coalescent (Identity a) where
  a `coalesce` b = sequenceA (coalesce <$> a <*> b)
