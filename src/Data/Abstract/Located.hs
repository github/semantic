{-# LANGUAGE TypeFamilies #-}
module Data.Abstract.Located where

import Control.Abstract.Evaluator
import Data.AST
import Data.Record

-- TODO: Dependencies
type Provenance = Record Location

data Located location = Located { provenance :: !Provenance, location :: location }

newtype LocatedValue value = LocatedValue { getLocatedValue :: value }

instance AbstractValue (LocatedValue value) where
  type LocationFor (LocatedValue value) = Located (LocationFor value)
