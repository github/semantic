module Data.Abstract.Configuration where

import Data.Abstract.Environment
import Data.Abstract.Heap
import Data.Abstract.Live

-- | A single point in a program’s execution.
data Configuration term location cell value = Configuration
  { configurationTerm        :: term                     -- ^ The “instruction,” i.e. the current term to evaluate.
  , configurationRoots       :: Live location            -- ^ The set of rooted addresses.
  , configurationEnvironment :: Environment location     -- ^ The environment binding any free variables in 'configurationTerm'.
  , configurationHeap        :: Heap location cell value -- ^ The heap of values.
  }
  deriving (Eq, Ord, Show)
