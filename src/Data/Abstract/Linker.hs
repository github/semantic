{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Linker where

import Data.Semigroup
import GHC.Generics
import qualified Data.Map as Map

newtype Linker v = Linker { unLinker :: Map.Map FilePath v }
  deriving (Eq, Foldable, Functor, Generic1, Monoid, Ord, Semigroup, Show, Traversable)

linkerLookup :: FilePath -> Linker v -> Maybe v
linkerLookup k = Map.lookup k . unLinker

linkerInsert :: FilePath -> v -> Linker v -> Linker v
linkerInsert k v = Linker . Map.insert k v . unLinker
