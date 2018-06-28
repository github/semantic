{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Exports
    ( Exports
    , aliases
    , insert
    , null
    , toEnvironment
    ) where

import Data.Abstract.Environment (Environment, unpairs, newEnv)
import Data.Abstract.Name
import qualified Data.Map as Map
import Prelude hiding (null)
import Prologue hiding (null)

-- | A map of export names to an alias & address tuple.
newtype Exports address = Exports { unExports :: Map.Map Name (Name, Maybe address) }
  deriving (Eq, Lower, Monoid, Ord, Semigroup)

null :: Exports address -> Bool
null = Map.null . unExports

toEnvironment :: Exports address -> Environment address
toEnvironment exports = newEnv (unpairs (mapMaybe sequenceA (toList (unExports exports))))

insert :: Name -> Name -> Maybe address -> Exports address -> Exports address
insert name alias address = Exports . Map.insert name (alias, address) . unExports

-- TODO: Should we filter for duplicates here?
aliases :: Exports address -> [(Name, Name)]
aliases = Map.toList . fmap fst . unExports


instance Show address => Show (Exports address) where
  showsPrec d = showsUnaryWith showsPrec "Exports" d . Map.toList . unExports
