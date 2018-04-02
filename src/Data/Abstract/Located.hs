{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
module Data.Abstract.Located where

import Control.Abstract.Addressable
import Control.Effect
import Control.Monad.Effect.Reader
import Data.Abstract.Address
import Data.Abstract.Origin
import Prologue

data Located location term = Located { location :: location, origin :: !(SomeOrigin term) }

deriving instance (Eq location, Eq (Base term ())) => Eq (Located location term)
deriving instance (Ord location, Ord (Base term ())) => Ord (Located location term)
deriving instance (Show location, Show (Base term ())) => Show (Located location term)

instance (Location location, Ord (Base term ())) => Location (Located location term) where
  type Cell (Located location term) = Cell location

instance ( Effectful m
         , Member (Reader (SomeOrigin term)) effects
         , MonadAddressable location (m effects)
         , Ord (Base term ())
         )
      => MonadAddressable (Located location term) (m effects) where
  derefCell (Address (Located loc _)) = derefCell (Address loc)

  allocLoc name = Located <$> allocLoc name <*> raise ask
