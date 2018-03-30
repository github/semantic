{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.ModuleTable
( ModuleName
, ModuleTable (..)
, lookup
, insert
, fromList
, toPairs
) where

import Data.Abstract.Module
import qualified Data.Map as Map
import Data.Semigroup
import GHC.Generics (Generic1)
import Prelude hiding (lookup)

newtype ModuleTable a = ModuleTable { unModuleTable :: Map.Map ModuleName a }
  deriving (Eq, Foldable, Functor, Generic1, Monoid, Ord, Semigroup, Show, Traversable)

lookup :: ModuleName -> ModuleTable a -> Maybe a
lookup k = Map.lookup k . unModuleTable

insert :: ModuleName -> a -> ModuleTable a -> ModuleTable a
insert k v ModuleTable{..} = ModuleTable (Map.insert k v unModuleTable)


-- | Construct a 'ModuleTable' from a list of 'Module's.
fromList :: [Module term] -> ModuleTable [Module term]
fromList = ModuleTable . Map.fromListWith (<>) . map toEntry
  where toEntry m = (moduleName m, [m])

toPairs :: ModuleTable a -> [(ModuleName, a)]
toPairs = Map.toList . unModuleTable
