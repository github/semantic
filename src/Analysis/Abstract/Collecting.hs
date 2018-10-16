module Analysis.Abstract.Collecting
( providingLiveSet
) where

import Control.Abstract
import Prologue

providingLiveSet :: Carrier sig m => Evaluator term address value (ReaderC (Live address) (Evaluator term address value m)) a -> Evaluator term address value m a
providingLiveSet = runReader lowerBound . runEvaluator
