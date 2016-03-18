module Data.Aligned
( Aligned (..)
, alignThis
, alignThat
, alignThese
) where

import Data.Bifunctor.Join
import Data.Bifunctor.These

-- | A functor over either or both sides of a list of computations.
newtype Aligned f recur = Aligned { unAligned :: Join These [f recur] }

alignThis :: [f recur] -> Aligned f recur
alignThis = Aligned . Join . This

alignThat :: [f recur] -> Aligned f recur
alignThat = Aligned . Join . That

alignThese :: [f recur] -> [f recur] -> Aligned f recur
alignThese = ((Aligned . Join) .) . These
