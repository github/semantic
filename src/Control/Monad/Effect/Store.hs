{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilyDependencies, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.Store
( assign
, MonadStore(..)
, modifyStore
) where

import Control.Monad.Effect
import Control.Monad.Effect.State
import Data.Abstract.Address
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Pointed
import Data.Semigroup

assign :: (Ord (LocationFor a), Semigroup (Cell (LocationFor a) a), Pointed (Cell (LocationFor a)), MonadStore a m) => Address (LocationFor a) a -> a -> m ()
assign = (modifyStore .) . storeInsert


-- | 'Monad's offering a readable & writable 'Store' of values for specific 'Address'es.
class Monad m => MonadStore a m where
  -- | Get the current store.
  getStore :: m (Store (LocationFor a) a)

  -- | Update the current store.
  putStore :: Store (LocationFor a) a -> m ()

instance (State (Store (LocationFor a) a) :< fs) => MonadStore a (Eff fs) where
  getStore = get
  putStore = put

modifyStore :: MonadStore a m => (Store (LocationFor a) a -> Store (LocationFor a) a) -> m ()
modifyStore f = getStore >>= putStore . f
