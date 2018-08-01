{-# LANGUAGE TypeOperators #-}
module Analysis.Abstract.Collecting
( collectingTerms
, providingLiveSet
) where

import Control.Abstract
import Prologue

-- | An analysis performing GC after every instruction.
collectingTerms :: ( Member (Reader (Live address)) effects
                   , Member (Allocator address value) effects
                   , Ord address
                   , ValueRoots address value
                   )
                => SubtermAlgebra (Base term) term (TermEvaluator term address value effects value)
                -> SubtermAlgebra (Base term) term (TermEvaluator term address value effects value)
collectingTerms recur term = do
  roots <- TermEvaluator askRoots
  v <- recur term
  v <$ TermEvaluator (gc (roots <> valueRoots v))


providingLiveSet :: (Effectful (m address value), PureEffects effects) => m address value (Reader (Live address) ': effects) a -> m address value effects a
providingLiveSet = runReader lowerBound
