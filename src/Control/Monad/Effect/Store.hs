{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeFamilyDependencies, TypeFamilies, TypeOperators, UndecidableInstances #-}
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


class Monad m => MonadStore a m where
  getStore :: m (Store (LocationFor a) a)
  putStore :: Store (LocationFor a) a -> m ()

instance (State (Store (LocationFor a) a) :< fs) => MonadStore a (Eff fs) where
  getStore = get
  putStore = put

modifyStore :: MonadStore a m => (Store (LocationFor a) a -> Store (LocationFor a) a) -> m ()
modifyStore f = getStore >>= putStore . f
