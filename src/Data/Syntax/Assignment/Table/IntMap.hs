module Data.Syntax.Assignment.Table.IntMap where
import qualified Data.IntMap as IntMap

data Table i a = Table { tableAddresses :: [i], tableBranches :: IntMap.IntMap a }
  deriving (Eq, Foldable, Functor, Show, Traversable)

singleton :: Enum i => i -> a -> Table i a
singleton i a = Table [i] (IntMap.singleton (fromEnum i) a)
