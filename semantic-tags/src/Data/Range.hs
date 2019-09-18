{-# LANGUAGE DeriveGeneric #-}
module Data.Range
( Range(..)
, rangeLength
, intersectsRange
, subtractRange
) where

import GHC.Generics (Generic)

-- | A half-open interval of integers, defined by start & end indices.
data Range = Range { start :: {-# UNPACK #-} !Int, end :: {-# UNPACK #-} !Int }
  deriving (Eq, Generic, Ord)

-- | Return the length of the range.
rangeLength :: Range -> Int
rangeLength range = end range - start range

-- | Test two ranges for intersection.
intersectsRange :: Range -> Range -> Bool
intersectsRange range1 range2 = start range1 < end range2 && start range2 < end range1

subtractRange :: Range -> Range -> Range
subtractRange range1 range2 = Range (start range1) (end range1 - rangeLength (Range (start range2) (max (end range1) (end range2))))


-- Instances

-- $
-- prop> a <> (b <> c) === (a <> b) <> (c :: Range)
instance Semigroup Range where
  Range start1 end1 <> Range start2 end2 = Range (min start1 start2) (max end1 end2)

instance Show Range where
  showsPrec _ r = showChar '[' . shows (start r) . showString " .. " . shows (end r) . showChar ']'


-- $setup
-- >>> import Test.QuickCheck
-- >>> instance Arbitrary Range where arbitrary = Range <$> arbitrary <*> arbitrary ; shrink (Range s e) = Range <$> shrink s <*> shrink e
