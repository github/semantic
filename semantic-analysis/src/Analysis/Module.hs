module Analysis.Module
( Module(..)
, ModuleSet(..)
) where

import           Analysis.Name
import qualified Data.Map as Map
import qualified Data.Set as Set

data Module a = Module
  { body    :: Map.Map Name a -> a
  , imports :: Set.Set Name
  , exports :: Map.Map Name a
  , unknown :: Set.Set Name
  }

newtype ModuleSet a = ModuleSet { getModuleSet :: Map.Map Name (Module a) }
