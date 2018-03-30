{-# LANGUAGE TypeFamilies #-}
module Data.Abstract.Located where

import Control.Abstract.Addressable
import Data.Abstract.Address
import Data.Abstract.Origin

data Located location term = Located { location :: location, origin :: !(SomeOrigin term) }
  deriving (Eq, Ord, Show)

instance (Location location, Ord term) => Location (Located location term) where
  type Cell (Located location term) = Cell location

instance (MonadAddressable location m, MonadOrigin term m, Ord term) => MonadAddressable (Located location term) m where
  derefCell (Address (Located loc _)) = derefCell (Address loc)

  allocLoc name = Located <$> allocLoc name <*> askOrigin
