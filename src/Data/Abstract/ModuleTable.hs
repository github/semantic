{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.ModuleTable
( ModulePath
, ModuleTable (..)
, singleton
, lookup
, member
, modulePathsInDir
, insert
, keys
, fromModules
, toPairs
, LoadStack (..)
, loadStackPush
, loadStackPop
) where

import Data.Abstract.Module
import qualified Data.Map as Map
import Data.Semigroup
import Data.Semilattice.Lower
import Prologue
import System.FilePath.Posix
import GHC.Generics (Generic1)
import Prelude hiding (lookup)

newtype ModuleTable a = ModuleTable { unModuleTable :: Map.Map ModulePath a }
  deriving (Eq, Foldable, Functor, Generic1, Lower, Monoid, Ord, Semigroup, Show, Traversable)

singleton :: ModulePath -> a -> ModuleTable a
singleton name = ModuleTable . Map.singleton name

modulePathsInDir :: FilePath -> ModuleTable a -> [ModulePath]
modulePathsInDir k = filter (\e -> k == takeDirectory e) . Map.keys . unModuleTable

lookup :: ModulePath -> ModuleTable a -> Maybe a
lookup k = Map.lookup k . unModuleTable

member :: ModulePath -> ModuleTable a -> Bool
member k = Map.member k . unModuleTable

insert :: ModulePath -> a -> ModuleTable a -> ModuleTable a
insert k v = ModuleTable . Map.insert k v . unModuleTable

keys :: ModuleTable a -> [ModulePath]
keys = Map.keys . unModuleTable

-- | Construct a 'ModuleTable' from a list of 'Module's.
fromModules :: [Module term] -> ModuleTable (NonEmpty (Module term))
fromModules modules = ModuleTable (Map.fromListWith (<>) (map toEntry modules))
  where toEntry m = (modulePath (moduleInfo m), m:|[])

toPairs :: ModuleTable a -> [(ModulePath, a)]
toPairs = Map.toList . unModuleTable


-- | Stack of module paths used to help break circular loads/imports.
newtype LoadStack = LoadStack { unLoadStack :: [ModulePath] }
  deriving (Eq, Lower, Monoid, Ord, Semigroup, Show)

loadStackPush :: ModulePath -> LoadStack -> LoadStack
loadStackPush x LoadStack{..} = LoadStack (x : unLoadStack)

loadStackPop :: LoadStack -> LoadStack
loadStackPop (LoadStack []) = LoadStack []
loadStackPop (LoadStack (_:xs)) = LoadStack xs
