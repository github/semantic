module Control.Abstract.Configuration
( getConfiguration
) where

import Control.Abstract.Environment
import Control.Abstract.Heap
import Control.Abstract.Roots
import Control.Abstract.TermEvaluator

-- | Get the current 'Configuration' with a passed-in term.
getConfiguration :: (Member (Reader (Live address)) effects, Member (Env address) effects, Member (State (Heap address value)) effects)
                 => term
                 -> TermEvaluator term address value effects (Configuration term address value)
getConfiguration term = Configuration term <$> TermEvaluator askRoots <*> TermEvaluator getEvalContext <*> TermEvaluator getHeap

