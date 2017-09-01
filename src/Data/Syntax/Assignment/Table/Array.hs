module Data.Syntax.Assignment.Table.Array
( Table(tableAddresses)
) where

import Data.Array
import Data.Foldable (toList)
import Data.Functor.Classes

data Table i a = Table { tableAddresses :: [i], tableBranches :: Array i (Maybe a) }


instance (Ix i, Show i) => Show1 (Table i) where
  liftShowsPrec spA slA d Table{..} = showsBinaryWith showsPrec (const (liftShowList spA slA)) "Table" d tableAddresses (assocs tableBranches >>= \ (sym, a) -> (,) sym <$> toList a)
