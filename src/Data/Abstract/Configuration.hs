module Data.Abstract.Configuration ( Configuration (..) ) where

import Data.Abstract.Environment
import Data.Abstract.Heap
import Data.Abstract.Live

-- | A single point in a program’s execution.
data Configuration term address value = Configuration
  { configurationTerm    :: term                -- ^ The “instruction,” i.e. the current term to evaluate.
  , configurationRoots   :: Live address        -- ^ The set of rooted addresses.
  , configurationHeap    :: Heap address address value  -- ^ The heap of values.
  }
  deriving (Eq, Ord, Show)
