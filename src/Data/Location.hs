{-# LANGUAGE DeriveAnyClass #-}

module Data.Location
  ( Location(..)
  , Span(..)
  , Range(..)
  ) where

import Prologue (Generic, NFData (..))

import Data.JSON.Fields
import Data.Range
import Data.Span

data Location
  = Location
  { locationByteRange :: {-# UNPACK #-} Range
  , locationSpan  :: {-# UNPACK #-} Span
  }
  deriving (Eq, Ord, Show, Generic, NFData)

instance ToJSONFields Location where
  toJSONFields Location{..} = toJSONFields locationByteRange <> toJSONFields locationSpan

instance Semigroup Location where
  (Location r1 sp1) <> (Location r2 sp2) = Location (r1 <> r2) (sp1 <> sp2)
