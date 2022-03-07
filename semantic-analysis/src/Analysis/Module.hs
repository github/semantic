module Analysis.Module
( Module(..)
) where

import           Analysis.Name
import qualified Data.Map as Map
import qualified Data.Set as Set

data Module a = Module
  { body    :: Map.Map Name a -> a
  , imports :: Set.Set Name
  , exports :: Map.Map Name a
  }
