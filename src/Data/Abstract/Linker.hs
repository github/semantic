{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Linker where

import Data.Semigroup
import GHC.Generics
import qualified Data.Map as Map


newtype Linker a = Linker { unLinker :: Map.Map FilePath a }
  deriving (Eq, Foldable, Functor, Generic1, Monoid, Ord, Semigroup, Show, Traversable)

linkerLookup :: FilePath -> Linker a -> Maybe a
linkerLookup k = Map.lookup k . unLinker

linkerInsert :: FilePath -> a -> Linker a -> Linker a
linkerInsert k v Linker{..} = Linker (Map.insert k v unLinker)
