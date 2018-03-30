{-# LANGUAGE TypeFamilies #-}
module Data.Abstract.Located where

import Control.Abstract.Addressable
import Data.Abstract.Address
import Data.Range
import Data.Record
import Data.Span

-- TODO: Dependencies
type Provenance = Record '[Range, Span]


class Monad m => MonadProvenance m where
  askProvenance :: m Provenance


data Located location = Located { location :: location, provenance :: !Provenance }
  deriving (Eq, Ord, Show)

instance Location location => Location (Located location) where
  type Cell (Located location) = Cell location

instance (MonadAddressable location m, MonadProvenance m) => MonadAddressable (Located location) m where
  derefCell (Address (Located loc _)) = derefCell (Address loc)

  allocLoc name = Located <$> allocLoc name <*> askProvenance
