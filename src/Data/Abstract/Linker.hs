{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Linker where

import Data.Semigroup
import GHC.Generics
import Data.ByteString
import qualified Data.Map as Map

type ModuleName = ByteString

newtype Linker a = Linker { unLinker :: Map.Map ModuleName a }
  deriving (Eq, Foldable, Functor, Generic1, Monoid, Ord, Semigroup, Show, Traversable)

linkerLookup :: ModuleName -> Linker a -> Maybe a
linkerLookup k = Map.lookup k . unLinker

linkerInsert :: ModuleName -> a -> Linker a -> Linker a
linkerInsert k v Linker{..} = Linker (Map.insert k v unLinker)
