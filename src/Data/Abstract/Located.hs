{-# LANGUAGE TypeFamilies #-}
module Data.Abstract.Located where

import Control.Abstract.Evaluator
import Control.Abstract.Value
import Data.Abstract.Address
import Data.Bifunctor
import Data.Range
import Data.Record
import Data.Span
import Prelude hiding (null)
import Prologue hiding (hash, null)

-- TODO: Dependencies
type Provenance = Record '[Range, Span]


data Located location = Located { provenance :: !Provenance, location :: location }
  deriving (Eq, Ord, Show)

instance Location location => Location (Located location) where
  type Cell (Located location) = Cell location


newtype LocatedValue value = LocatedValue { unLocatedValue :: value }
  deriving (Eq, Ord, Show)

instance AbstractValue (LocatedValue value) where
  type LocationFor (LocatedValue value) = Located (LocationFor value)

instance MonadValue value m => MonadValue (LocatedValue value) m where
  unit = LocatedValue <$> unit
  null = LocatedValue <$> null
  integer = fmap LocatedValue . integer
  float = fmap LocatedValue . float
  rational = fmap LocatedValue . rational
  boolean = fmap LocatedValue . boolean
  multiple = fmap LocatedValue . multiple . map unLocatedValue
  string = fmap LocatedValue . string
  symbol = fmap LocatedValue . symbol
  array = fmap LocatedValue . array . map unLocatedValue
  hash = fmap LocatedValue . hash . map (bimap unLocatedValue unLocatedValue)
  ifthenelse = ifthenelse . unLocatedValue
  kvPair = fmap (fmap LocatedValue) . (kvPair `on` unLocatedValue)
  -- klass name vals env = LocatedValue <$> klass name (map unLocatedValue vals) (fmap unLocatedValue env)
