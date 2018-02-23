{-# LANGUAGE DeriveAnyClass #-}
module Data.Abstract.Linker where

import Data.Semigroup
import GHC.Generics
import qualified Data.Map as Map

data Linker t a = Linker { linkerValues :: Map.Map FilePath a, linkerTerms :: Map.Map FilePath t }
  deriving (Eq, Foldable, Functor, Generic1, Ord, Semigroup, Show, Traversable)

instance Monoid (Linker t a) where
  mempty = Linker Map.empty Map.empty
  mappend (Linker v1 t1) (Linker v2 t2) = Linker (Map.union v1 v2) (Map.union t1 t2)

linkerLookupValue :: FilePath -> Linker t a -> Maybe a
linkerLookupValue k = Map.lookup k . linkerValues

linkerLookupTerm :: FilePath -> Linker t a -> Maybe t
linkerLookupTerm k = Map.lookup k . linkerTerms

linkerInsert :: FilePath -> a -> Linker t a -> Linker t a
linkerInsert k v Linker{..} = Linker (Map.insert k v linkerValues) (Map.delete k linkerTerms)


newtype Linker' a = Linker' { unLinker :: Map.Map FilePath a }
  deriving (Eq, Foldable, Functor, Generic1, Monoid, Ord, Semigroup, Show, Traversable)

linkerLookup :: FilePath -> Linker' a -> Maybe a
linkerLookup k = Map.lookup k . unLinker
