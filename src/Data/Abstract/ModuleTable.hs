{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.ModuleTable
  ( ModuleName
  , ModuleTable (..)
  , moduleTableLookup
  , moduleTableInsert
  , fromList
  ) where

import Data.Abstract.Module
import Data.Semigroup
import GHC.Generics (Generic1)
import qualified Data.Map as Map

newtype ModuleTable a = ModuleTable { unModuleTable :: Map.Map ModuleName a }
  deriving (Eq, Foldable, Functor, Generic1, Monoid, Ord, Semigroup, Show, Traversable)

moduleTableLookup :: ModuleName -> ModuleTable a -> Maybe a
moduleTableLookup k = Map.lookup k . unModuleTable

moduleTableInsert :: ModuleName -> a -> ModuleTable a -> ModuleTable a
moduleTableInsert k v ModuleTable{..} = ModuleTable (Map.insert k v unModuleTable)


fromList :: [Module term] -> ModuleTable [Module term]
fromList modules = ModuleTable (Map.fromListWith (<>) (map ((,) . moduleName <*> pure) modules))
