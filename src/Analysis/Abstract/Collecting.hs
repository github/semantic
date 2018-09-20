{-# LANGUAGE TypeOperators #-}
module Analysis.Abstract.Collecting
( collectingTerms
, providingLiveSet
) where

import Control.Abstract
import Prologue

-- | An analysis performing GC after every instruction.
collectingTerms :: ( Member (Reader (Live address)) effects
                   , Member (State (Heap address value)) effects
                   , Ord address
                   , ValueRoots address value
                   )
                => SubtermAlgebra (Base term) term (Evaluator term address value effects value)
                -> SubtermAlgebra (Base term) term (Evaluator term address value effects value)
collectingTerms recur term = do
  roots <- askRoots
  v <- recur term
  v <$ gc (roots <> valueRoots v)


providingLiveSet :: PureEffects effects => Evaluator term address value (Reader (Live address) ': effects) a -> Evaluator term address value effects a
providingLiveSet = runReader lowerBound
