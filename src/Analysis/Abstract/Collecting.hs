module Analysis.Abstract.Collecting
( providingLiveSet
) where

import Control.Abstract
import Prologue

providingLiveSet :: Carrier sig m => Evaluator term address value (ReaderC (Live address) (Eff m)) a -> Evaluator term address value m a
providingLiveSet = raiseHandler (runReader lowerBound)
