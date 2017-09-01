module Data.Syntax.Assignment.Table.Array
( Table
, tableAddresses
, singleton
, fromListWith
, toList
, lookup
) where

import Control.Arrow ((&&&))
import Data.Array
import Data.Functor.Classes
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Max(..), Min(..), sconcat)
import GHC.Stack
import Prelude hiding (lookup)

data Table i a = Table { tableAddressSet :: IntSet.IntSet, tableBranches :: Array i (Maybe a) }
  deriving (Eq, Foldable, Functor, Show, Traversable)

tableAddresses :: Enum i => Table i a -> [i]
tableAddresses = fmap toEnum . IntSet.toList . tableAddressSet


singleton :: (Enum i, Ix i) => i -> a -> Table i a
singleton i a = Table (IntSet.singleton (fromEnum i)) (listArray (i, i) [Just a])

fromListWith :: (HasCallStack, Enum i, Ix i) => (a -> a -> a) -> [(i, a)] -> Table i a
fromListWith _ [] = error "fromList: empty list of associations"
fromListWith with assocs@(a:as) = Table (IntSet.fromList (fromEnum . fst <$> assocs)) (accumArray merge Nothing (getMin mn, getMax mx) assocs)
  where (mn, mx) = sconcat ((Min &&& Max) . fst <$> a:|as)
        merge Nothing b = Just b
        merge (Just a) b = Just (with a b)

toList :: (Enum i, Ix i) => Table i a -> [(i, a)]
toList Table{..} = toEnum <$> IntSet.toList tableAddressSet >>= \ addr -> maybe [] (pure . (,) addr) (tableBranches ! addr)


lookup :: Ix i => i -> Table i a -> Maybe a
lookup i Table{..}
  | bounds tableBranches `inRange` i = tableBranches ! i
  | otherwise                        = Nothing


instance (Enum i, Ix i, Show i) => Show1 (Table i) where
  liftShowsPrec spA slA d t = showsBinaryWith showsPrec (const (liftShowList spA slA)) "Table" d (tableAddresses t) (toList t)
