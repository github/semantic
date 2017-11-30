{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilyDependencies, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Store
( Precise(..)
, Monovariant(..)
, MonadAddress(alloc, Cell)
, Store(..)
, storeLookup
, storeLookupAll
, storeRestrict
, Address(..)
, deref
, assign
, MonadStore(..)
, modifyStore
) where

import Abstract.Address
import Abstract.FreeVariables
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

newtype Store l a = Store { unStore :: Map.Map (Address l a) (Cell l a) }
  deriving (Semigroup, Monoid)

storeLookup :: Ord l => Address l a -> Store l a -> Maybe (Cell l a)
storeLookup = (. unStore) . Map.lookup

storeLookupAll :: (Ord l, Foldable (Cell l)) => Address l a -> Store l a -> Maybe [a]
storeLookupAll address = fmap toList . storeLookup address

storeInsert :: (Ord l, Semigroup (Cell l a), Pointed (Cell l)) => Address l a -> a -> Store l a -> Store l a
storeInsert = (((Store .) . (. unStore)) .) . (. point) . Map.insertWith (<>)

storeSize :: Store l a -> Int
storeSize = Map.size . unStore

storeRestrict :: Ord l => Store l a -> Set.Set (Address l a) -> Store l a
storeRestrict (Store m) roots = Store (Map.filterWithKey (\ address _ -> address `Set.member` roots) m)


assign :: (Ord l, Semigroup (Cell l a), Pointed (Cell l), MonadStore l a m) => Address l a -> a -> m ()
assign = (modifyStore .) . storeInsert


class Monad m => MonadStore l a m where
  getStore :: m (Store l a)
  putStore :: Store l a -> m ()

instance (State (Store l a) :< fs) => MonadStore l a (Eff fs) where
  getStore = get
  putStore = put

modifyStore :: MonadStore l a m => (Store l a -> Store l a) -> m ()
modifyStore f = getStore >>= putStore . f


class (Ord l, Pointed (Cell l), Monad m) => MonadAddress l m where
  type Cell (l :: *) = (res :: * -> *) | res -> l

  deref :: (MonadStore l a m, MonadFail m) => Address l a -> m a

  alloc :: MonadStore l a m => Name -> m (Address l a)


newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

allocPrecise :: Store Precise a -> Address Precise a
allocPrecise = Address . Precise . storeSize

newtype I a = I { unI :: a }
  deriving (Eq, Generic1, Ord, Show)

instance Monad m => MonadAddress Precise m where
  type Cell Precise = I

  deref = maybe uninitializedAddress (pure . unI) <=< flip fmap getStore . storeLookup

  alloc _ = fmap allocPrecise getStore


newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord, Show)

instance (Alternative m, Monad m) => MonadAddress Monovariant m where
  type Cell Monovariant = Set.Set

  deref = asum . maybe [] (map pure . toList) <=< flip fmap getStore . storeLookup

  alloc = pure . Address . Monovariant


uninitializedAddress :: MonadFail m => m a
uninitializedAddress = fail "uninitialized address"


instance Semigroup (I a) where
  (<>) = const

instance Foldable I where
  foldMap f = f . unI

instance Functor I where
  fmap f = I . f . unI

instance Traversable I where
  traverse f = fmap I . f . unI

instance Pointed I where
  point = I

instance Eq1 I where liftEq = genericLiftEq
instance Ord1 I where liftCompare = genericLiftCompare
instance Show1 I where liftShowsPrec = genericLiftShowsPrec


instance Foldable (Cell l) => Foldable (Store l) where
  foldMap = (. unStore) . foldMap . foldMap

instance (Ord l, Functor (Cell l)) => Functor (Store l) where
  fmap f = Store . Map.mapKeys (Address . unAddress) . fmap (fmap f) . unStore

instance (Ord l, Traversable (Cell l)) => Traversable (Store l) where
  traverse f = fmap (Store . Map.mapKeys (Address . unAddress)) . traverse (traverse f) . unStore


instance (Eq l, Eq1 (Cell l)) => Eq1 (Store l) where
  liftEq eq (Store m1) (Store m2) = liftEq2 (liftEq eq) (liftEq eq) m1 m2

instance (Eq a, Eq l, Eq1 (Cell l)) => Eq (Store l a) where
  (==) = eq1

instance (Ord l, Ord1 (Cell l)) => Ord1 (Store l) where
  liftCompare compareA (Store m1) (Store m2) = liftCompare2 (liftCompare compareA) (liftCompare compareA) m1 m2

instance (Ord a, Ord l, Ord1 (Cell l)) => Ord (Store l a) where
  compare = compare1

instance (Show l, Show1 (Cell l)) => Show1 (Store l) where
  liftShowsPrec sp sl d (Store m) = showsUnaryWith (liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl)) "Store" d m

instance (Show a, Show l, Show1 (Cell l)) => Show (Store l a) where
  showsPrec = showsPrec1
