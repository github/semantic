{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.ModuleTable
  ( ModuleTable (..)
  , moduleTableLookup
  , moduleTableInsert
  )where

import Data.Semigroup
import GHC.Generics
import qualified Data.Map as Map

newtype ModuleTable a = ModuleTable { unModuleTable :: Map.Map FilePath a }
  deriving (Eq, Foldable, Functor, Generic1, Monoid, Ord, Semigroup, Show, Traversable)

moduleTableLookup :: FilePath -> ModuleTable a -> Maybe a
moduleTableLookup k = Map.lookup k . unModuleTable

moduleTableInsert :: FilePath -> a -> ModuleTable a -> ModuleTable a
moduleTableInsert k v ModuleTable{..} = ModuleTable (Map.insert k v unModuleTable)
