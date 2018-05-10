module Control.Abstract.Configuration
( Configuration(..)
, Live
, getConfiguration
) where

import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Control.Abstract.Roots
import Data.Abstract.Configuration

-- | Get the current 'Configuration' with a passed-in term.
getConfiguration :: Members '[Reader (Live location value), State (Environment location value), State (Heap location (Cell location) value)] effects => term -> Evaluator location value effects (Configuration term location (Cell location) value)
getConfiguration term = Configuration term <$> askRoots <*> getEnv <*> getHeap
