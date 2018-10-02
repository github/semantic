{-# LANGUAGE TypeOperators #-}
module Analysis.Abstract.Collecting
( providingLiveSet
) where

import Control.Abstract
import Prologue

providingLiveSet :: PureEffects effects => Evaluator term address value (Reader (Live address) ': effects) a -> Evaluator term address value effects a
providingLiveSet = runReader lowerBound
