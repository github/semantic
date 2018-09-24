module Data.Location
  ( Location(..)
  , Span(..)
  , Range(..)
  , DiffAnnotation
  ) where

import Data.JSON.Fields
import Data.Range
import Data.Span

type DiffAnnotation a = (a, Location)

-- | A location specified as possibly-empty intervals of bytes and line/column positions.
-- type Location = '[Range, Span]

data Location
  = Location
  { locationByteRange :: {-# UNPACK #-} Range
  , locationSpan  :: {-# UNPACK #-} Span
  }
  deriving (Eq, Ord, Show)

instance ToJSONFields Location where
  toJSONFields Location{..} = toJSONFields locationByteRange <> toJSONFields locationSpan

instance Semigroup Location where
  (Location r1 sp1) <> (Location r2 sp2) = Location (r1 <> r2) (sp1 <> sp2)
