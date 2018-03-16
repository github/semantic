{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies #-}
module Data.Abstract.Environment where

import Data.Abstract.Address
import Data.Abstract.FreeVariables
import Data.Abstract.Live
import Data.Align
import qualified Data.Map as Map
import Data.Semigroup.Reducer
import GHC.Exts (IsList (..))
import Prologue
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | A map of names to addresses that represents the evaluation environment.
newtype Environment l a = Environment { unEnvironment :: NonEmpty (Map.Map Name (Address l a)) }
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

instance Eq l => Eq1 (Environment l) where liftEq = genericLiftEq
instance Ord l => Ord1 (Environment l) where liftCompare = genericLiftCompare
instance Show l => Show1 (Environment l) where liftShowsPrec = genericLiftShowsPrec

-- | The provided list will be put into an Environment with one member, so fromList is total
--   (despite NonEmpty's instance being partial). Don't pass in multiple Addresses for the
--   same Name or you violate the axiom that toList . fromList == id.
instance IsList (Environment l a) where
  type Item (Environment l a) = (Name, Address l a)
  fromList xs                   = Environment (Map.fromList xs :| [])
  toList (Environment (x :| _)) = Map.toList x

instance Semigroup (Environment l a) where
  Environment (a :| as) <> Environment (b :| bs) =
    Environment ((a <> b) :| alignWith (mergeThese (<>)) as bs)

instance Reducer (Name, Address l a) (Environment l a) where
  unit a = Environment (unit a :| [])

-- Possibly unlawful. If this breaks, you get to keep both pieces.
instance Monoid (Environment l a) where
  mappend = (<>)
  mempty  = Environment (mempty :| [])

envPush :: Environment l a -> Environment l a
envPush (Environment (a :| as)) = Environment (mempty :| a : as)

envPop :: Environment l a -> Environment l a
envPop (Environment (_ :| [])) = mempty
envPop (Environment (_ :| a : as)) = Environment (a :| as)

envHead :: Environment l a -> Environment l a
envHead (Environment (a :| _)) = Environment (a :| [])

-- TODO: Test the flattening behavior
envPairs :: Environment l a -> [(Name, Address l a)]
envPairs = Map.toList . fold . unEnvironment

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

-- | Retrieve the 'Live' set of addresses to which the given free variable names are bound.
--
--   Unbound names are silently dropped.
envRoots :: (Ord l, Foldable t) => Environment l a -> t Name -> Live l a
envRoots env = foldMap (maybe mempty liveSingleton . flip envLookup env)

-- TODO, VERY BROKEN, DON'T COMMIT THIS: needs to prefer inwardly-bound names to handle scoping
envAll :: (Ord l) => Environment l a -> Live l a
envAll (Environment env) = Live $ Set.fromList (foldMap Map.elems env)
