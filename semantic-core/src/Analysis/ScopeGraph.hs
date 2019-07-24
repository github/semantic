module Analysis.ScopeGraph
( ScopeGraph
, Entry(..)
) where

import           Data.Loc
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

data Entry = Entry
  { entrySymbol :: Text.Text
  , entryLoc    :: Loc
  }

type ScopeGraph = Map.Map Entry (Set.Set Entry)
