module Analysis.Abstract.Configuration
( getConfiguration
) where

import Control.Abstract.Environment
import Control.Abstract.Heap
import Control.Abstract.Roots
import Control.Abstract.TermEvaluator
import Data.Abstract.Heap

-- | Get the current 'Configuration' with a passed-in term.
getConfiguration :: (Member (Reader (Live address)) effects, Member (Env address) effects, Member (State (EvaluatorHeap address value)) effects)
                 => term
                 -> TermEvaluator term address value effects (Configuration term address value)
getConfiguration term = Configuration term <$> TermEvaluator askRoots <*> TermEvaluator getEvalContext <*> TermEvaluator getHeap

