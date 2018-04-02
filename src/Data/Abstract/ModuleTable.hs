{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.ModuleTable
( ModuleName
, ModuleTable (..)
, singleton
, lookup
, member
, moduleTableKeysForDir
, insert
, fromModules
, toPairs
) where

import Data.Abstract.Module
import qualified Data.Map as Map
import Data.Semigroup
import Prologue
import System.FilePath.Posix
import GHC.Generics (Generic1)
import Prelude hiding (lookup)

newtype ModuleTable a = ModuleTable { unModuleTable :: Map.Map ModuleName a }
  deriving (Eq, Foldable, Functor, Generic1, Monoid, Ord, Semigroup, Show, Traversable)

singleton :: ModuleName -> a -> ModuleTable a
singleton name = ModuleTable . Map.singleton name

moduleTableKeysForDir :: FilePath -> ModuleTable a -> [ModuleName]
moduleTableKeysForDir k = filter (\e -> k == takeDirectory e) . Map.keys . unModuleTable

lookup :: ModuleName -> ModuleTable a -> Maybe a
lookup k = Map.lookup k . unModuleTable

member :: ModuleName -> ModuleTable a -> Bool
member k = Map.member k . unModuleTable

insert :: ModuleName -> a -> ModuleTable a -> ModuleTable a
insert k v = ModuleTable . Map.insert k v . unModuleTable


-- | Construct a 'ModuleTable' from a list of 'Module's.
fromModules :: [Module term] -> ModuleTable [Module term]
fromModules = ModuleTable . Map.fromListWith (<>) . map toEntry
  where toEntry m = (modulePath (moduleInfo m), [m])

toPairs :: ModuleTable a -> [(ModuleName, a)]
toPairs = Map.toList . unModuleTable
