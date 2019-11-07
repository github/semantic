{-# LANGUAGE DeriveGeneric, DeriveTraversable, DerivingVia, RecordWildCards #-}
module Assigning.Assignment.Table
( Table(tableAddresses)
, singleton
, fromListWith
, toPairs
, lookup
) where

import Prologue
import Prelude hiding (lookup)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

data Table i a = Table { tableAddresses :: [i], tableBranches :: IntMap a }
  deriving (Eq, Foldable, Functor, Show, Traversable, Generic)
  deriving Monoid via GenericMonoid (Table i a)

singleton :: Enum i => i -> a -> Table i a
singleton i a = Table [i] (IntMap.singleton (fromEnum i) a)

fromListWith :: Enum i => (a -> a -> a) -> [(i, a)] -> Table i a
fromListWith with assocs = Table (toEnum <$> IntSet.toList (IntSet.fromList (fromEnum . fst <$> assocs))) (IntMap.fromListWith with (first fromEnum <$> assocs))

toPairs :: Enum i => Table i a -> [(i, a)]
toPairs Table{..} = first toEnum <$> IntMap.toList tableBranches


lookup :: Enum i => i -> Table i a -> Maybe a
lookup i = IntMap.lookup (fromEnum i) . tableBranches

instance (Enum i, Semigroup a) => Semigroup (Table i a) where
  (Table i1 b1) <> (Table i2 b2) = Table (i1 <> i2) (IntMap.unionWith (<>) b1 b2)

instance (Enum i, Show i) => Show1 (Table i) where
  liftShowsPrec spA slA d t = showsBinaryWith showsPrec (const (liftShowList spA slA)) "Table" d (tableAddresses t) (toPairs t)
