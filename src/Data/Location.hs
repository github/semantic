{-# LANGUAGE DeriveAnyClass, DerivingVia #-}

module Data.Location
  ( Location(..)
  , Span(..)
  , Range(..)
  ) where

import Prologue

import Data.JSON.Fields
import Data.Range
import Data.Span

data Location
  = Location
  { locationByteRange :: {-# UNPACK #-} Range
  , locationSpan  :: {-# UNPACK #-} Span
  }
  deriving (Eq, Ord, Show, Generic, NFData)
  deriving Semigroup via GenericSemigroup Location

instance ToJSONFields Location where
  toJSONFields Location{..} = toJSONFields locationByteRange <> toJSONFields locationSpan
