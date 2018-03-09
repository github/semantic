{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.ModuleTable
  ( ModuleName
  , ModuleTable (..)
  , moduleTableLookup
  , moduleTableInsert
  ) where

import Data.Abstract.FreeVariables
import Data.Semigroup
import GHC.Generics
import qualified Data.Map as Map


type ModuleName = Name

newtype ModuleTable a = ModuleTable { unModuleTable :: Map.Map ModuleName a }
  deriving (Eq, Foldable, Functor, Generic1, Monoid, Ord, Semigroup, Show, Traversable)

moduleTableLookup :: ModuleName -> ModuleTable a -> Maybe a
moduleTableLookup k = Map.lookup k . unModuleTable

moduleTableInsert :: ModuleName -> a -> ModuleTable a -> ModuleTable a
moduleTableInsert k v ModuleTable{..} = ModuleTable (Map.insert k v unModuleTable)
