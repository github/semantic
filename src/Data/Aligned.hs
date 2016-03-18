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

-- | Construct a functor aligning a list of children at left with nothing at right.
alignThis :: [f recur] -> Aligned f recur
alignThis = Aligned . Join . This

-- | Construct a functor aligning a list of children at right with nothing at left.
alignThat :: [f recur] -> Aligned f recur
alignThat = Aligned . Join . That

-- | Construct a functor aligning two lists of children.
alignThese :: [f recur] -> [f recur] -> Aligned f recur
alignThese = ((Aligned . Join) .) . These
