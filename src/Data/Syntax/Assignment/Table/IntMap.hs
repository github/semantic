module Data.Syntax.Assignment.Table.IntMap
( Table(tableAddresses)
, singleton
, fromListWith
, toList
, lookup
) where

import Data.Bifunctor (first)
import Data.Functor.Classes
import qualified Data.IntMap as IntMap
import Prelude hiding (lookup)

data Table i a = Table { tableAddresses :: [i], tableBranches :: IntMap.IntMap a }
  deriving (Eq, Foldable, Functor, Show, Traversable)

singleton :: Enum i => i -> a -> Table i a
singleton i a = Table [i] (IntMap.singleton (fromEnum i) a)

fromListWith :: Enum i => (a -> a -> a) -> [(i, a)] -> Table i a
fromListWith with assocs = Table (fst <$> assocs) (IntMap.fromListWith with (first fromEnum <$> assocs))

toList :: Enum i => Table i a -> [(i, a)]
toList Table{..} = first toEnum <$> IntMap.toList tableBranches


lookup :: Enum i => i -> Table i a -> Maybe a
lookup i = IntMap.lookup (fromEnum i) . tableBranches


instance (Enum i, Show i) => Show1 (Table i) where
  liftShowsPrec spA slA d t = showsBinaryWith showsPrec (const (liftShowList spA slA)) "Table" d (tableAddresses t) (toList t)
