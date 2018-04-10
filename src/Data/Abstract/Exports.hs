{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Exports
    ( Exports
    , aliases
    , insert
    , null
    , toEnvironment
    ) where

import Prelude hiding (null)
import Prologue hiding (null)
import Data.Abstract.Address
import Data.Abstract.Environment (Environment)
import Data.Abstract.FreeVariables
import qualified Data.Map as Map
import Data.Semigroup.Reducer

-- | A map of export names to an alias & address tuple.
newtype Exports l a = Exports { unExports :: Map.Map Name (Name, Maybe (Address l a)) }
  deriving (Eq, Foldable, Functor, Generic1, Monoid, Ord, Semigroup, Show, Traversable)

null :: Exports l a -> Bool
null = Map.null . unExports

toEnvironment :: Exports l a -> Environment l a
toEnvironment = Map.foldMapWithKey buildEnv . unExports where
  buildEnv _ (_, Nothing) = mempty
  buildEnv _ (n, Just a)  = unit (n, a)

insert :: Name -> Name -> Maybe (Address l a) -> Exports l a -> Exports l a
insert name alias address = Exports . Map.insert name (alias, address) . unExports

-- TODO: Should we filter for duplicates here?
aliases :: Exports l a -> [(Name, Name)]
aliases = Map.toList . fmap fst . unExports

instance Eq l => Eq1 (Exports l) where liftEq = genericLiftEq
instance Ord l => Ord1 (Exports l) where liftCompare = genericLiftCompare
instance Show l => Show1 (Exports l) where liftShowsPrec = genericLiftShowsPrec
