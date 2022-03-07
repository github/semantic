module Analysis.Module
( Module(..)
) where

import           Analysis.Name
import qualified Data.Map as Map

data Module a = Module
  { body :: Map.Map Name a -> a
  }
