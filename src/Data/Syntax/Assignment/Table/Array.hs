module Data.Syntax.Assignment.Table.Array
( Table(tableAddresses)
, singleton
, fromListWith
) where

import Control.Arrow ((&&&))
import Data.Array
import Data.Foldable (toList)
import Data.Functor.Classes
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Max(..), Min(..), sconcat)
import GHC.Stack

data Table i a = Table { tableAddresses :: [i], tableBranches :: Array i (Maybe a) }
  deriving (Foldable, Functor, Traversable)

singleton :: Ix i => i -> a -> Table i a
singleton i a = Table [i] (listArray (i, i) [Just a])

fromListWith :: (HasCallStack, Ix i) => (a -> a -> a) -> [(i, a)] -> Table i a
fromListWith _ [] = error "fromList: empty list of associations"
fromListWith with assocs@(a:as) = Table (fst <$> assocs) (accumArray merge Nothing (getMin mn, getMax mx) assocs)
  where (mn, mx) = sconcat ((Min &&& Max) . fst <$> a:|as)
        merge Nothing b = Just b
        merge (Just a) b = Just (with a b)


instance (Ix i, Show i) => Show1 (Table i) where
  liftShowsPrec spA slA d Table{..} = showsBinaryWith showsPrec (const (liftShowList spA slA)) "Table" d tableAddresses (tableAddresses >>= \ addr -> (,) addr <$> toList (tableBranches ! addr))
