module Data.Abstract.Configuration where

import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.Heap
import Data.Abstract.Live

-- | A single point in a program’s execution.
data Configuration term location value = Configuration
  { configurationTerm        :: term                                -- ^ The “instruction,” i.e. the current term to evaluate.
  , configurationRoots       :: Live location value                 -- ^ The set of rooted addresses.
  , configurationEnvironment :: Environment location value          -- ^ The environment binding any free variables in 'configurationTerm'.
  , configurationHeap        :: Heap (Cell location) location value -- ^ The heap of values.
  }

deriving instance (Eq   term, Eq   location, Eq   value, Eq   (Cell location value)) => Eq   (Configuration term location value)
deriving instance (Ord  term, Ord  location, Ord  value, Ord  (Cell location value)) => Ord  (Configuration term location value)
deriving instance (Show term, Show location, Show value, Show (Cell location value)) => Show (Configuration term location value)
