module Data.Abstract.Located where

import Data.AST
import Data.Record

-- TODO: Dependencies
type Provenance = Record Location

data Located location = Located { provenance :: !Provenance, location :: location }

newtype LocatedValue value = LocatedValue { getLocatedValue :: value }
