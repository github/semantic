module Control.Abstract.Configuration
( Configuration(..)
, Live
, getConfiguration
) where

import Control.Abstract.Addressable
import Control.Abstract.Environment
import Control.Abstract.Heap
import Control.Abstract.Roots
import Control.Abstract.TermEvaluator
import Data.Abstract.Configuration

-- | Get the current 'Configuration' with a passed-in term.
getConfiguration :: (Member (Reader (Live address)) effects, Member (State (Environment address)) effects, Member (State (Heap address (Cell address) value)) effects) => term -> TermEvaluator term address value effects (Configuration term address (Cell address) value)
getConfiguration term = Configuration term <$> TermEvaluator askRoots <*> TermEvaluator getEnv <*> TermEvaluator getHeap
