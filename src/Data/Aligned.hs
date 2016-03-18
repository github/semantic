{-# LANGUAGE PatternSynonyms #-}
module Data.Aligned
( Aligned (..)
, pattern AlignThis
, pattern AlignThat
, alignThese
) where

import Data.Bifunctor.Join
import Data.Bifunctor.These

-- | A functor over either or both sides of a list of computations.
newtype Aligned f recur = Aligned { unAligned :: Join These [f recur] }

-- | Construct a functor aligning a list of children at left with nothing at right.
pattern AlignThis a = Aligned (Join (This a))

-- | Construct a functor aligning a list of children at right with nothing at left.
pattern AlignThat b = Aligned (Join (That b))

-- | Construct a functor aligning two lists of children.
alignThese :: [f recur] -> [f recur] -> Aligned f recur
alignThese = ((Aligned . Join) .) . These
