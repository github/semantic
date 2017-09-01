module Data.Syntax.Assignment.Table.IntMap
( Table
, tableAddresses
, singleton
, fromListWith
, toList
, lookup
) where

import Data.Bifunctor (first)
import Data.Functor.Classes
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Prelude hiding (lookup)

data Table i a = Table { tableAddressSet :: IntSet.IntSet, tableBranches :: IntMap.IntMap a }
  deriving (Eq, Foldable, Functor, Show, Traversable)

tableAddresses :: Enum i => Table i a -> [i]
tableAddresses = fmap toEnum . IntSet.toList . tableAddressSet


singleton :: Enum i => i -> a -> Table i a
singleton i a = Table (IntSet.singleton i') (IntMap.singleton i' a)
  where i' = fromEnum i

fromListWith :: Enum i => (a -> a -> a) -> [(i, a)] -> Table i a
fromListWith with assocs = Table (IntSet.fromList (fst <$> assocs')) (IntMap.fromListWith with assocs')
  where assocs' = first fromEnum <$> assocs

toList :: Enum i => Table i a -> [(i, a)]
toList Table{..} = first toEnum <$> IntMap.toList tableBranches


lookup :: Enum i => i -> Table i a -> Maybe a
lookup i = IntMap.lookup (fromEnum i) . tableBranches


instance (Enum i, Monoid a) => Monoid (Table i a) where
  mempty = Table mempty mempty
  mappend (Table i1 b1) (Table i2 b2) = Table (i1 `mappend` i2) (IntMap.unionWith mappend b1 b2)

instance (Enum i, Show i) => Show1 (Table i) where
  liftShowsPrec spA slA d t = showsBinaryWith showsPrec (const (liftShowList spA slA)) "Table" d (tableAddresses t) (toList t)
