module Analysis.ScopeGraph
( ScopeGraph
, Entry(..)
) where

import           Data.Loc
import qualified Data.Map as Map
import qualified Data.Set as Set

data Entry = Entry
  { entrySymbol :: String -- FIXME: Text
  , entryLoc    :: Loc
  }

type ScopeGraph = Map.Map Entry (Set.Set Entry)
