{-# LANGUAGE DeriveGeneric, RankNTypes #-}
module Source.Range
( Range(..)
, point
, rangeLength
, subtractRange
  -- * Lenses
, start_
, end_
) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Semilattice.Lower (Lower(..))
import GHC.Generics (Generic)

-- | A 0-indexed, half-open interval of integers, defined by start & end indices.
data Range = Range
  { start :: {-# UNPACK #-} !Int
  , end   :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Generic, Ord, Show)

instance Hashable Range
instance NFData   Range

-- $
-- prop> a <> (b <> c) === (a <> b) <> (c :: Range)
instance Semigroup Range where
  Range start1 end1 <> Range start2 end2 = Range (min start1 start2) (max end1 end2)

instance Lower Range where
  lowerBound = Range 0 0


-- | Construct a 'Range' with a given value for both its start and end indices.
point :: Int -> Range
point i = Range i i

-- | Return the length of the range.
rangeLength :: Range -> Int
rangeLength range = end range - start range

subtractRange :: Range -> Range -> Range
subtractRange range1 range2 = Range (start range1) (end range1 - rangeLength (Range (start range2) (max (end range1) (end range2))))


start_, end_ :: Lens' Range Int
start_ = lens start (\r s -> r { start = s })
end_   = lens end   (\r e -> r { end   = e })


type Lens' s a = forall f . Functor f => (a -> f a) -> (s -> f s)

lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens get put afa s = fmap (put s) (afa (get s))
{-# INLINE lens #-}


-- $setup
-- >>> import Test.QuickCheck
-- >>> instance Arbitrary Range where arbitrary = Range <$> arbitrary <*> arbitrary ; shrink (Range s e) = Range <$> shrink s <*> shrink e
