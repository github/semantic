module Analysis.ScopeGraph
( ScopeGraph
, Entry(..)
) where

import           Data.Loc
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text (Text)

data Entry = Entry
  { entrySymbol :: Text
  , entryLoc    :: Loc
  }
  deriving (Eq, Ord, Show)

type ScopeGraph = Map.Map Entry (Set.Set Entry)
