{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Store
( Precise(..)
, Monovariant(..)
, MonadAddress(alloc, Cell)
, Store(..)
, storeLookup
, storeLookupAll
, storeRestrict
, Address(..)
, Set(..)
, deref
, assign
, MonadStore(..)
, modifyStore
) where

import Abstract.Set
import Abstract.FreeVariables
import Control.Applicative
import Control.Monad ((<=<))
import Control.Monad.Effect
import Control.Monad.Effect.State
import Control.Monad.Fail
import Data.Foldable (asum, toList)
import Data.Functor.Classes
import qualified Data.Map as Map
import Data.Pointed
import Data.Semigroup
import Prelude hiding (fail)

newtype Store l a = Store { unStore :: Map.Map (Address l a) (Cell l a) }
  deriving (Semigroup, Monoid)

newtype Address l a = Address { unAddress :: l }
  deriving (Eq, Ord, Show)

storeLookup :: Ord l => Address l a -> Store l a -> Maybe (Cell l a)
storeLookup = (. unStore) . Map.lookup

storeLookupAll :: (Ord l, Foldable (Cell l)) => Address l a -> Store l a -> Maybe [a]
storeLookupAll address = fmap toList . storeLookup address

storeInsert :: (Ord l, Semigroup (Cell l a), Pointed (Cell l)) => Address l a -> a -> Store l a -> Store l a
storeInsert = (((Store .) . (. unStore)) .) . (. point) . Map.insertWith (<>)

storeSize :: Store l a -> Int
storeSize = Map.size . unStore

storeRestrict :: Ord l => Store l a -> Set (Address l a) -> Store l a
storeRestrict (Store m) roots = Store (Map.filterWithKey (\ address _ -> address `member` roots) m)


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
  type Cell l :: * -> *

  deref :: (MonadStore l a m, MonadFail m) => Address l a -> m a

  alloc :: MonadStore l a m => Name -> m (Address l a)


newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord, Show)

allocPrecise :: Store Precise a -> Address Precise a
allocPrecise = Address . Precise . storeSize

newtype I a = I { unI :: a }
  deriving (Eq, Ord, Show)

instance Monad m => MonadAddress Precise m where
  type Cell Precise = I

  deref = maybe uninitializedAddress (pure . unI) <=< flip fmap getStore . storeLookup

  alloc _ = fmap allocPrecise getStore


newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord, Show)

instance (Alternative m, Monad m) => MonadAddress Monovariant m where
  type Cell Monovariant = Set

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

instance Eq1 I where
  liftEq eq (I a) (I b) = eq a b

instance Ord1 I where
  liftCompare comp (I a) (I b) = comp a b

instance Show1 I where
  liftShowsPrec sp _ d (I a) = sp d a

instance Foldable (Address l) where
  foldMap _ = mempty

instance Functor (Address l) where
  fmap _ = Address . unAddress

instance Traversable (Address l) where
  traverse _ = fmap Address . pure . unAddress


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

instance Eq2 Address where
  liftEq2 eqL _ (Address a) (Address b) = eqL a b

instance Eq l => Eq1 (Address l) where
  liftEq = liftEq2 (==)

instance (Ord l, Ord1 (Cell l)) => Ord1 (Store l) where
  liftCompare compareA (Store m1) (Store m2) = liftCompare2 (liftCompare compareA) (liftCompare compareA) m1 m2

instance (Ord a, Ord l, Ord1 (Cell l)) => Ord (Store l a) where
  compare = compare1

instance Ord2 Address where
  liftCompare2 compareL _ (Address a) (Address b) = compareL a b

instance Ord l => Ord1 (Address l) where
  liftCompare = liftCompare2 compare

instance (Show l, Show1 (Cell l)) => Show1 (Store l) where
  liftShowsPrec sp sl d (Store m) = showsUnaryWith (liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl)) "Store" d m

instance (Show a, Show l, Show1 (Cell l)) => Show (Store l a) where
  showsPrec = showsPrec1

instance Show2 Address where
  liftShowsPrec2 spL _ _ _ d = showsUnaryWith spL "Address" d . unAddress

instance Show l => Show1 (Address l) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
