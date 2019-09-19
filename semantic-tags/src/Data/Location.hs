{-# LANGUAGE DeriveGeneric #-}
module Data.Location
( Location(..)
, Span(..)
, Range(..)
) where

import Control.Applicative (liftA2)
import Data.Range
import Data.Span
import GHC.Generics (Generic)
import TreeSitter.Unmarshal

data Location = Location
  { locationByteRange :: {-# UNPACK #-} !Range
  , locationSpan      :: {-# UNPACK #-} !Span
  }
  deriving (Eq, Generic, Ord, Show)

instance Unmarshal Location where
  unmarshalNodes = liftA2 Location <$> unmarshalNodes <*> unmarshalNodes
