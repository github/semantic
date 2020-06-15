module Analysis.Abstract.Collecting
( providingLiveSet
) where

import Control.Abstract
import Control.Carrier.Reader
import Data.Semilattice.Lower

providingLiveSet :: Evaluator term address value (ReaderC (Live address) m) a -> Evaluator term address value m a
providingLiveSet = raiseHandler (runReader lowerBound)
