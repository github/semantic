{-# LANGUAGE DeriveAnyClass #-}
module Data.Range where

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

-- | Test two ranges for intersection.
intersectsRange :: Range -> Range -> Bool
intersectsRange range1 range2 = start range1 < end range2 && start range2 < end range1

-- | Return a range that contains both the given ranges.
unionRange :: Range -> Range -> Range
unionRange (Range start1 end1) (Range start2 end2) = Range (min start1 start2) (max end1 end2)


-- Instances

instance Semigroup Range where
  a <> b = unionRange a b

instance Ord Range where
  a <= b = start a <= start b

instance Listable Range where
  tiers = cons2 Range
