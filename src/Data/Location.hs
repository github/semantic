{-# LANGUAGE DeriveAnyClass, DerivingVia #-}

module Data.Location
  ( Location(..)
  , Span(..)
  , Range(..)
  ) where

import Prologue

import Control.Lens.Lens
import Data.Span
import Source.Range

data Location
  = Location
  { locationByteRange :: {-# UNPACK #-} Range
  , locationSpan  :: {-# UNPACK #-} Span
  }
  deriving (Eq, Ord, Show, Generic, NFData)
  deriving Semigroup via GenericSemigroup Location

instance HasSpan Location where
  span = lens locationSpan (\l s -> l { locationSpan = s })
  {-# INLINE span #-}
