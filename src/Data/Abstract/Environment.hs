{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving #-}
module Data.Abstract.Environment where

import Data.Abstract.Address
import Data.Abstract.FreeVariables
import Data.Abstract.Live
import Data.Semigroup.Reducer
import Prologue
import qualified Data.Map as Map

-- | A map of names to addresses that represents the evaluation environment.
newtype Environment l a = Environment { unEnvironment :: Map.Map Name (Address l a) }
  deriving (Eq, Foldable, Functor, Generic1, Monoid, Ord, Semigroup, Show, Traversable)

deriving instance Reducer (Name, Address l a) (Environment l a)

-- | A map of export names to an alias & address tuple.
newtype Exports l a = Exports { unExports :: Map.Map Name (Name, Maybe (Address l a)) }
  deriving (Eq, Foldable, Functor, Generic1, Monoid, Ord, Semigroup, Show, Traversable)

-- | Lookup a 'Name' in the environment.
envLookup :: Name -> Environment l a -> Maybe (Address l a)
envLookup k = Map.lookup k . unEnvironment

-- | Insert a 'Name' in the environment.
envInsert :: Name -> Address l a -> Environment l a -> Environment l a
envInsert name value (Environment m) = Environment (Map.insert name value m)

bindEnv :: (Ord l, Foldable t) => t Name -> Environment l a -> Environment l a
bindEnv names env = foldMap envForName names
  where envForName name = maybe mempty (curry unit name) (envLookup name env)

exportInsert :: Name -> (Name, Maybe (Address l a)) -> Exports l a -> Exports l a
exportInsert name value = Exports . Map.insert name value . unExports

-- | Retrieve the 'Live' set of addresses to which the given free variable names are bound.
--
--   Unbound names are silently dropped.
envRoots :: (Ord l, Foldable t) => Environment l a -> t Name -> Live l a
envRoots env = foldr ((<>) . maybe mempty liveSingleton . flip envLookup env) mempty


-- Instances

instance Eq l => Eq1 (Environment l) where liftEq = genericLiftEq
instance Ord l => Ord1 (Environment l) where liftCompare = genericLiftCompare
instance Show l => Show1 (Environment l) where liftShowsPrec = genericLiftShowsPrec

instance Eq l => Eq1 (Exports l) where liftEq = genericLiftEq
instance Ord l => Ord1 (Exports l) where liftCompare = genericLiftCompare
instance Show l => Show1 (Exports l) where liftShowsPrec = genericLiftShowsPrec
