{-# LANGUAGE DeriveAnyClass #-}
module Data.Range where

import Data.List.NonEmpty (nonEmpty)
import Data.Semigroup
import Prologue
import Test.LeanCheck

-- | A half-open interval of integers, defined by start & end indices.
data Range = Range { start :: {-# UNPACK #-} !Int, end :: {-# UNPACK #-} !Int }
  deriving (Eq, Show, Generic, NFData)

-- | Return the length of the range.
rangeLength :: Range -> Int
rangeLength range = end range - start range

-- | Offset a range by a constant delta.
offsetRange :: Range -> Int -> Range
offsetRange a b = Range (start a + b) (end a + b)

-- | Divide a range in two at the given coordinate.
--
--   Passing a coordinate that does not lie between start and end will result in one of the ranges being empty.
divideRange :: Range -> Int -> (Range, Range)
divideRange Range{..} at = (Range start divider, Range divider end)
  where divider = max (min end at) start

-- | Return Just the last index from a non-empty range, or if the range is empty, Nothing.
maybeLastIndex :: Range -> Maybe Int
maybeLastIndex (Range start end) | start == end = Nothing
maybeLastIndex (Range _ end) = Just $ end - 1

-- | Test two ranges for intersection.
intersectsRange :: Range -> Range -> Bool
intersectsRange range1 range2 = start range1 < end range2 && start range2 < end range1

-- Return the (possibly empty, possibly ill-formed) intersection of two ranges.
intersectionRange :: Range -> Range -> Range
intersectionRange range1 range2 = Range (max (start range1) (start range2)) (min (end range1) (end range2))

-- | Return a range that contains both the given ranges.
unionRange :: Range -> Range -> Range
unionRange (Range start1 end1) (Range start2 end2) = Range (min start1 start2) (max end1 end2)

-- | Return a range that contains all the ranges in a Foldable, or the passed Range if the Foldable is empty.
unionRangesFrom :: Foldable f => Range -> f Range -> Range
unionRangesFrom range = maybe range sconcat . nonEmpty . toList


-- Instances

instance Semigroup Range where
  a <> b = unionRange a b

instance Ord Range where
  a <= b = start a <= start b

instance Listable Range where
  tiers = cons2 Range
