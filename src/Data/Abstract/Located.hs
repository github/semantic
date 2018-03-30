{-# LANGUAGE TypeFamilies #-}
module Data.Abstract.Located where

import Control.Abstract.Addressable
import Data.Abstract.Address
import Data.Abstract.Origin

class Monad m => MonadOrigin m where
  askOrigin :: m Origin


data Located location = Located { location :: location, provenance :: !Origin }
  deriving (Eq, Ord, Show)

instance Location location => Location (Located location) where
  type Cell (Located location) = Cell location

instance (MonadAddressable location m, MonadOrigin m) => MonadAddressable (Located location) m where
  derefCell (Address (Located loc _)) = derefCell (Address loc)

  allocLoc name = Located <$> allocLoc name <*> askOrigin
