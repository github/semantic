{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving #-}
module Data.Abstract.Environment where

import Data.Abstract.Address
import Data.Abstract.FreeVariables
import Data.Abstract.Live
import Data.Align
import qualified Data.Map as Map
import Data.Semigroup.Reducer
import Prologue
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | A map of names to addresses that represents the evaluation environment.
newtype Environment l a = Environment { unEnvironment :: NonEmpty (Map.Map Name (Address l a)) }
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

instance Semigroup (Environment l a) where
  Environment (a :| as) <> Environment (b :| bs) =
    Environment ((a <> b) :| alignWith (mergeThese (<>)) as bs)

instance Reducer (Name, Address l a) (Environment l a) where
  unit a = Environment (unit a :| [])

-- Possibly unlawful. If this breaks, you get to keep both pieces.
instance Monoid (Environment l a) where
  mappend = (<>)
  mempty  = Environment (mempty :| [])

-- TODO: Test the flattening behavior
envPairs :: Environment l a -> [(Name, Address l a)]
envPairs = Map.toList . fold . unEnvironment

-- | A map of export names to an alias & address tuple.
newtype Exports l a = Exports { unExports :: Map.Map Name (Name, Maybe (Address l a)) }
  deriving (Eq, Foldable, Functor, Generic1, Monoid, Ord, Semigroup, Show, Traversable)

-- | Lookup a 'Name' in the environment.
envLookup :: Name -> Environment l a -> Maybe (Address l a)
envLookup k = foldMapA (Map.lookup k) . unEnvironment

-- | Insert a 'Name' in the environment.
envInsert :: Name -> Address l a -> Environment l a -> Environment l a
envInsert name value (Environment (a :| as)) = Environment (Map.insert name value a :| as)

envDelete :: Name -> Environment l a -> Environment l a
envDelete name = Environment . fmap (Map.delete name) . unEnvironment

bindEnv :: (Ord l, Foldable t) => t Name -> Environment l a -> Environment l a
bindEnv names env = foldMap envForName names
  where envForName name = maybe mempty (curry unit name) (envLookup name env)

envNames :: Environment l a -> [Name]
envNames = fmap fst . envPairs

-- TODO: rename this, because it both filters and renames
envRename :: [(Name, Name)] -> Environment l a -> Environment l a
envRename pairs env = foldMap rename pairs where
  rename (k, v) = case envLookup k env of
    Nothing   -> mempty
    Just addr -> unit (v, addr)

-- TODO: change these to `insert` and `null` and add an export list, importing them qualified at the callsite

exportNull :: Exports l a -> Bool
exportNull = Map.null . unExports

exportsToEnv :: Exports l a -> Environment l a
exportsToEnv = Map.foldMapWithKey buildEnv . unExports where
  buildEnv _ (_, Nothing) = mempty
  buildEnv _ (n, Just a)  = unit (n, a)

exportInsert :: Name -> Name -> Maybe (Address l a) -> Exports l a -> Exports l a
exportInsert name alias address = Exports . Map.insert name (alias, address) . unExports

-- TODO: Should we filter for duplicates here?
exportAliases :: Exports l a -> [(Name, Name)]
exportAliases = Map.toList . fmap fst . unExports

-- | Retrieve the 'Live' set of addresses to which the given free variable names are bound.
--
--   Unbound names are silently dropped.
envRoots :: (Ord l, Foldable t) => Environment l a -> t Name -> Live l a
envRoots env = foldMap (maybe mempty liveSingleton . flip envLookup env)

-- TODO, VERY BROKEN, DON'T COMMIT THIS: needs to prefer inwardly-bound names to handle scoping
envAll :: (Ord l) => Environment l a -> Live l a
envAll (Environment env) = Live $ Set.fromList (foldMap Map.elems env)


-- Instances

instance Eq l => Eq1 (Environment l) where liftEq = genericLiftEq
instance Ord l => Ord1 (Environment l) where liftCompare = genericLiftCompare
instance Show l => Show1 (Environment l) where liftShowsPrec = genericLiftShowsPrec

instance Eq l => Eq1 (Exports l) where liftEq = genericLiftEq
instance Ord l => Ord1 (Exports l) where liftCompare = genericLiftCompare
instance Show l => Show1 (Exports l) where liftShowsPrec = genericLiftShowsPrec
