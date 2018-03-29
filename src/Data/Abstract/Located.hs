{-# LANGUAGE TypeFamilies #-}
module Data.Abstract.Located where

import Control.Abstract.Evaluator
import Data.Range
import Data.Record
import Data.Span

-- TODO: Dependencies
type Provenance = Record '[Range, Span]

data Located location = Located { provenance :: !Provenance, location :: location }
  deriving (Eq, Ord, Show)

newtype LocatedValue value = LocatedValue { getLocatedValue :: value }
  deriving (Eq, Ord, Show)

instance AbstractValue (LocatedValue value) where
  type LocationFor (LocatedValue value) = Located (LocationFor value)
