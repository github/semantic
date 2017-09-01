module Data.Syntax.Assignment.Table.IntMap where

import Data.Bifunctor (first)
import qualified Data.IntMap as IntMap

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
