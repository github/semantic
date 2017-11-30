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

import Abstract.Address
import Abstract.Environment
import Abstract.FreeVariables
import Abstract.Value
import Control.Applicative
import Control.Monad ((<=<))
import Control.Monad.Effect
import Control.Monad.Effect.State
import Control.Monad.Fail
import Data.Foldable (asum, toList)
import Data.Functor.Classes
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Ord.Generic
import Data.Functor.Classes.Show.Generic
import qualified Data.Map as Map
import Data.Pointed
import Data.Semigroup
import qualified Data.Set as Set
import GHC.Generics
import Prelude hiding (fail)

newtype Store l a = Store { unStore :: Map.Map l (Cell l a) }
  deriving (Generic1, Monoid, Semigroup)

deriving instance (Eq l, Eq (Cell l a)) => Eq (Store l a)
deriving instance (Ord l, Ord (Cell l a)) => Ord (Store l a)
deriving instance (Show l, Show (Cell l a)) => Show (Store l a)
instance (Eq l, Eq1 (Cell l)) => Eq1 (Store l) where liftEq = genericLiftEq
instance (Ord l, Ord1 (Cell l)) => Ord1 (Store l) where liftCompare = genericLiftCompare
instance (Show l, Show1 (Cell l)) => Show1 (Store l) where liftShowsPrec = genericLiftShowsPrec
deriving instance Foldable (Cell l) => Foldable (Store l)
deriving instance Functor (Cell l) => Functor (Store l)
deriving instance Traversable (Cell l) => Traversable (Store l)

storeLookup :: Ord l => Address l a -> Store l a -> Maybe (Cell l a)
storeLookup = (. unStore) . Map.lookup . unAddress

storeLookupAll :: (Ord l, Foldable (Cell l)) => Address l a -> Store l a -> Maybe [a]
storeLookupAll address = fmap toList . storeLookup address

storeInsert :: (Ord l, Semigroup (Cell l a), Pointed (Cell l)) => Address l a -> a -> Store l a -> Store l a
storeInsert = (((Store .) . (. unStore)) .) . (. point) . Map.insertWith (<>) . unAddress

storeSize :: Store l a -> Int
storeSize = Map.size . unStore

storeRestrict :: Ord l => Store l a -> Set.Set (Address l a) -> Store l a
storeRestrict (Store m) roots = Store (Map.filterWithKey (\ address _ -> Address address `Set.member` roots) m)

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
