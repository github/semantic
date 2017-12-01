{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Environment where

import Data.Abstract.Address
import Data.Abstract.FreeVariables
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Ord.Generic
import Data.Functor.Classes.Show.Generic
import qualified Data.Map as Map
import Data.Pointed
import Data.Semigroup
import qualified Data.Set as Set
import GHC.Generics


newtype Environment l a = Environment { unEnvironment :: Map.Map Name (Address l a) }
  deriving (Eq, Foldable, Functor, Generic1, Monoid, Ord, Semigroup, Show, Traversable)

envLookup :: Name -> Environment l a -> Maybe (Address l a)
envLookup = (. unEnvironment) . Map.lookup

envInsert :: Name -> Address l a -> Environment l a -> Environment l a
envInsert name value (Environment m) = Environment (Map.insert name value m)

envRoots :: (Ord l, Foldable t) => Environment l a -> t Name -> Set.Set (Address l a)
envRoots env = foldr ((<>) . maybe mempty point . flip envLookup env) mempty


-- Instances

instance Eq l => Eq1 (Environment l) where liftEq = genericLiftEq
instance Ord l => Ord1 (Environment l) where liftCompare = genericLiftCompare
instance Show l => Show1 (Environment l) where liftShowsPrec = genericLiftShowsPrec
