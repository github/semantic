{-# LANGUAGE DeriveGeneric #-}
module Data.Location
( Location(..)
, Span(..)
, Range(..)
) where

import Data.Range
import Data.Span
import GHC.Generics (Generic)

data Location = Location
  { locationByteRange :: {-# UNPACK #-} !Range
  , locationSpan      :: {-# UNPACK #-} !Span
  }
  deriving (Eq, Generic, Ord, Show)
