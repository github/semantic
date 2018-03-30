{-# LANGUAGE TypeFamilies #-}
module Data.Abstract.Located where

import Data.Abstract.Address
import Data.Range
import Data.Record
import Data.Span

-- TODO: Dependencies
type Provenance = Record '[Range, Span]


data Located location = Located { provenance :: !Provenance, location :: location }
  deriving (Eq, Ord, Show)

instance Location location => Location (Located location) where
  type Cell (Located location) = Cell location
