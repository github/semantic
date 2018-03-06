{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.ModuleTable where

import Data.Semigroup
import GHC.Generics
import qualified Data.Map as Map


newtype ModuleTable a = ModuleTable { unModuleTable :: Map.Map FilePath a }
  deriving (Eq, Foldable, Functor, Generic1, Monoid, Ord, Semigroup, Show, Traversable)

linkerLookup :: FilePath -> ModuleTable a -> Maybe a
linkerLookup k = Map.lookup k . unModuleTable

linkerInsert :: FilePath -> a -> ModuleTable a -> ModuleTable a
linkerInsert k v ModuleTable{..} = ModuleTable (Map.insert k v unModuleTable)
