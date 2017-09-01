module Data.Syntax.Assignment.Table.Array
( Table(tableAddresses)
, tableSingleton
) where

import Data.Array
import Data.Foldable (toList)
import Data.Functor.Classes

data Table i a = Table { tableAddresses :: [i], tableBranches :: Array i (Maybe a) }
  deriving (Foldable, Functor, Traversable)

tableSingleton :: Ix i => i -> a -> Table i a
tableSingleton i a = Table [i] (listArray (i, i) [Just a])


instance (Ix i, Show i) => Show1 (Table i) where
  liftShowsPrec spA slA d Table{..} = showsBinaryWith showsPrec (const (liftShowList spA slA)) "Table" d tableAddresses (tableAddresses >>= \ addr -> (,) addr <$> toList (tableBranches ! addr))
