{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
module Data.Abstract.Located where

import Control.Abstract.Addressable
import Control.Effect
import Control.Monad.Effect.Reader
import Data.Abstract.Address
import Data.Abstract.Origin
import Prologue

data Located location term = Located { location :: location, origin :: !(SomeOrigin term) }
  deriving (Eq, Ord, Show)

instance (Location location, Ord term) => Location (Located location term) where
  type Cell (Located location term) = Cell location

instance ( Effectful m
         , Member (Reader (SomeOrigin term)) effects
         , MonadAddressable location (m effects)
         , Ord term
         )
      => MonadAddressable (Located location term) (m effects) where
  derefCell (Address (Located loc _)) = derefCell (Address loc)

  allocLoc name = Located <$> allocLoc name <*> raise ask
