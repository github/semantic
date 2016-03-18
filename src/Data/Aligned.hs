module Data.Aligned
( Aligned (..)
) where

import Data.Bifunctor.Join
import Data.Bifunctor.These

-- | A functor over either or both sides of a list of computations.
newtype Aligned f recur = Aligned { unAligned :: Join These [f recur] }
