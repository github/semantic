module Data.Syntax.Assignment.Table.Array
( Table(tableAddresses)
) where

import Data.Array

data Table i a = Table { tableAddresses :: [i], tableBranches :: Array i (Maybe a) }
