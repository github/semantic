{-# LANGUAGE DeriveAnyClass #-}
module Data.Range
( Range(..)
, emptyRange
, rangeLength
, subtractRange
) where

import Prologue

import Data.Aeson
import Data.JSON.Fields

-- | A half-open interval of integers, defined by start & end indices.
data Range = Range { start :: {-# UNPACK #-} !Int, end :: {-# UNPACK #-} !Int }
  deriving (Eq, Generic, NFData, Ord)

emptyRange :: Range
emptyRange = Range 0 0

-- | Return the length of the range.
rangeLength :: Range -> Int
rangeLength range = end range - start range

subtractRange :: Range -> Range -> Range
subtractRange range1 range2 = Range (start range1) (end range1 - rangeLength (Range (start range2) (max (end range1) (end range2))))


-- Instances

-- | The associativity of this instance is specced in @Data.Range.Spec@.
instance Semigroup Range where
  Range start1 end1 <> Range start2 end2 = Range (min start1 start2) (max end1 end2)

instance Show Range where
  showsPrec _ Range{..} = showChar '[' . shows start . showString " .. " . shows end . showChar ']'

instance ToJSONFields Range where
  toJSONFields Range{..} = ["sourceRange" .= [ start, end ]]

instance Lower Range where
  lowerBound = Range 0 0
