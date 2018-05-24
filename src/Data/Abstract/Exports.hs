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
newtype Exports location = Exports { unExports :: Map.Map Name (Name, Maybe location) }
  deriving (Eq, Lower, Monoid, Ord, Semigroup)

null :: Exports location -> Bool
null = Map.null . unExports

toEnvironment :: Exports location -> Environment location
toEnvironment exports = unpairs (mapMaybe (traverse (fmap Address)) (toList (unExports exports)))

insert :: Name -> Name -> Maybe (Address location value) -> Exports location -> Exports location
insert name alias address = Exports . Map.insert name (alias, unAddress <$> address) . unExports

-- TODO: Should we filter for duplicates here?
aliases :: Exports location -> [(Name, Name)]
aliases = Map.toList . fmap fst . unExports


instance Show location => Show (Exports location) where
  showsPrec d = showsUnaryWith showsPrec "Exports" d . Map.toList . unExports
