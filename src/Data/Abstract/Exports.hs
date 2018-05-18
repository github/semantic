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
import Data.Abstract.Environment (Environment, unpairs)
import Data.Abstract.Name
import qualified Data.Map as Map
import Data.Semilattice.Lower

-- | A map of export names to an alias & address tuple.
newtype Exports location value = Exports { unExports :: Map.Map Name (Name, Maybe (Address location value)) }
  deriving (Eq, Lower, Monoid, Ord, Semigroup)

null :: Exports location value -> Bool
null = Map.null . unExports

toEnvironment :: Exports location value -> Environment location value
toEnvironment exports = unpairs (mapMaybe collectExport (toList (unExports exports)))
  where
    collectExport (_, Nothing) = Nothing
    collectExport (n, Just value)  = Just (n, value)

insert :: Name -> Name -> Maybe (Address location value) -> Exports location value -> Exports location value
insert name alias address = Exports . Map.insert name (alias, address) . unExports

-- TODO: Should we filter for duplicates here?
aliases :: Exports location value -> [(Name, Name)]
aliases = Map.toList . fmap fst . unExports


instance Show location => Show (Exports location value) where
  showsPrec d = showsUnaryWith showsPrec "Exports" d . Map.toList . unExports
