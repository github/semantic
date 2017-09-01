module Data.Syntax.Assignment.Table.Array
( Table(tableAddresses)
, singleton
) where

import Data.Array
import Data.Foldable (toList)
import Data.Functor.Classes

data Table i a = Table { tableAddresses :: [i], tableBranches :: Array i (Maybe a) }
  deriving (Foldable, Functor, Traversable)

singleton :: Ix i => i -> a -> Table i a
singleton i a = Table [i] (listArray (i, i) [Just a])


instance (Ix i, Show i) => Show1 (Table i) where
  liftShowsPrec spA slA d Table{..} = showsBinaryWith showsPrec (const (liftShowList spA slA)) "Table" d tableAddresses (tableAddresses >>= \ addr -> (,) addr <$> toList (tableBranches ! addr))
