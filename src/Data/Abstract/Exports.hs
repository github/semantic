{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Exports
    ( Exports
    , aliases
    , insert
    , null
    , toBindings
    ) where

import Data.Abstract.Environment (Bindings, unpairs)
import Data.Abstract.Name
import qualified Data.Map as Map
import Prelude hiding (null)
import Prologue hiding (null)

-- | A map of export names to an alias & address tuple.
newtype Exports address = Exports { unExports :: Map.Map Name (Name, Maybe address) }
  deriving (Eq, Lower, Monoid, Ord, Semigroup)

null :: Exports address -> Bool
null = Map.null . unExports

toBindings :: Exports address -> Bindings address
toBindings exports = unpairs (mapMaybe sequenceA (toList (unExports exports)))

-- TODO: Should inserts overwrite an existing value for a given name?
insert :: Name -> Name -> Maybe address -> Exports address -> Exports address
insert name alias address = Exports . Map.insert name (alias, address) . unExports

-- TODO: Should we filter for duplicates here?
aliases :: Exports address -> [(Name, Name)]
aliases = Map.toList . fmap fst . unExports


instance Show address => Show (Exports address) where
  showsPrec d = showsUnaryWith showsPrec "Exports" d . Map.toList . unExports
