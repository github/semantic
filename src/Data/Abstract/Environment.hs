{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving #-}
module Data.Abstract.Environment where

import Prologue
import Data.Abstract.Address
import Data.Abstract.FreeVariables
import Data.Abstract.Live
import qualified Data.Map as Map
import Data.Semigroup.Reducer
import qualified Data.Set as Set

-- | A map of names to addresses that represents the evaluation environment.
newtype Environment l a = Environment { unEnvironment :: Map.Map Name (Address l a) }
  deriving (Eq, Foldable, Functor, Generic1, Monoid, Ord, Semigroup, Show, Traversable)

deriving instance Reducer (Name, Address l a) (Environment l a)

-- | Lookup a 'Name' in the environment.
envLookup :: Name -> Environment l a -> Maybe (Address l a)
envLookup k = Map.lookup k . unEnvironment

-- | Insert a 'Name' in the environment.
envInsert :: Name -> Address l a -> Environment l a -> Environment l a
envInsert name value (Environment m) = Environment (Map.insert name value m)

envDelete :: Name -> Environment l a -> Environment l a
envDelete name = Environment . Map.delete name . unEnvironment

bindEnv :: Foldable t => t Name -> Environment l a -> Environment l a
bindEnv names env = foldMap envForName names
  where envForName name = maybe mempty (curry unit name) (envLookup name env)

bindExports :: Map Name (Name, Maybe (Address l a)) -> Environment l a -> Environment l a
bindExports aliases env = Environment pairs
  where
    pairs = Map.foldrWithKey (\name (alias, address) accum ->
      maybe accum (\v -> Map.insert alias v accum) (address <|> envLookup name env)) mempty aliases

-- | Retrieve the 'Live' set of addresses to which the given free variable names are bound.
--
--   Unbound names are silently dropped.
envRoots :: (Ord l, Foldable t) => Environment l a -> t Name -> Live l a
envRoots env = foldMap (maybe mempty liveSingleton . flip envLookup env)

envAll :: (Ord l) => Environment l a -> Live l a
envAll (Environment env) = Live $ Set.fromList (Map.elems env)

-- Instances

instance Eq l => Eq1 (Environment l) where liftEq = genericLiftEq
instance Ord l => Ord1 (Environment l) where liftCompare = genericLiftCompare
instance Show l => Show1 (Environment l) where liftShowsPrec = genericLiftShowsPrec
