{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeFamilyDependencies, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Store
( Precise(..)
, Monovariant(..)
, MonadAddress(alloc)
, Cell
, Store(..)
, storeLookup
, storeLookupAll
, storeRestrict
, envLookupOrAlloc'
, envLookupOrAlloc
, Address(..)
, deref
, assign
, MonadStore(..)
, modifyStore
) where

import Control.Applicative
import Control.Monad ((<=<))
import Control.Monad.Effect
import Control.Monad.Effect.State
import Control.Monad.Fail
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Foldable (asum, toList)
import Data.Pointed
import Data.Semigroup
import Prelude hiding (fail)

envLookupOrAlloc' ::
                 ( FreeVariables t
                 , Semigroup (Cell (LocationFor a) a)
                 , MonadStore a m
                 , MonadAddress (LocationFor a) m
                 )
                 => t -> Environment (LocationFor a) a -> a -> m (Name, Address (LocationFor a) a)
envLookupOrAlloc' term = let [name] = toList (freeVariables term) in
                         envLookupOrAlloc name

envLookupOrAlloc ::
                 ( Semigroup (Cell (LocationFor a) a)
                 , MonadStore a m
                 , MonadAddress (LocationFor a) m
                 )
                 => Name -> Environment (LocationFor a) a -> a -> m (Name, Address (LocationFor a) a)
envLookupOrAlloc name env v = do
  a <- maybe (alloc name) pure (envLookup name env)
  assign a v
  pure (name, a)


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


class (Ord l, Pointed (Cell l), Monad m) => MonadAddress l m where
  deref :: (MonadStore a m, MonadFail m, l ~ LocationFor a) => Address l a -> m a

  alloc :: (MonadStore a m, l ~ LocationFor a) => Name -> m (Address l a)


allocPrecise :: Store Precise a -> Address Precise a
allocPrecise = Address . Precise . storeSize

instance Monad m => MonadAddress Precise m where
  deref = maybe uninitializedAddress (pure . unLatest) <=< flip fmap getStore . storeLookup

  alloc _ = fmap allocPrecise getStore


instance (Alternative m, Monad m) => MonadAddress Monovariant m where
  deref = asum . maybe [] (map pure . toList) <=< flip fmap getStore . storeLookup

  alloc = pure . Address . Monovariant


uninitializedAddress :: MonadFail m => m a
uninitializedAddress = fail "uninitialized address"
