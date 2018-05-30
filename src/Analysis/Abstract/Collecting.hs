{-# LANGUAGE TypeOperators #-}
module Analysis.Abstract.Collecting
( collectingTerms
, providingLiveSet
) where

import Control.Abstract
import Data.Abstract.Heap
import Data.Abstract.Live
import Data.Semilattice.Lower
import Prologue

-- | An analysis performing GC after every instruction.
collectingTerms :: ( Foldable (Cell address)
                   , Member (Reader (Live address)) effects
                   , Member (State (Heap address (Cell address) value)) effects
                   , Ord address
                   , ValueRoots address value
                   )
                => SubtermAlgebra (Base term) term (TermEvaluator term address value effects value)
                -> SubtermAlgebra (Base term) term (TermEvaluator term address value effects value)
collectingTerms recur term = do
  roots <- TermEvaluator askRoots
  v <- recur term
  v <$ TermEvaluator (modifyHeap (gc (roots <> valueRoots v)))

-- | Collect any addresses in the heap not rooted in or reachable from the given 'Live' set.
gc :: ( Ord address
      , Foldable (Cell address)
      , ValueRoots address value
      )
   => Live address                      -- ^ The set of addresses to consider rooted.
   -> Heap address (Cell address) value -- ^ A heap to collect unreachable addresses within.
   -> Heap address (Cell address) value -- ^ A garbage-collected heap.
gc roots heap = heapRestrict heap (reachable roots heap)

-- | Compute the set of addresses reachable from a given root set in a given heap.
reachable :: ( Ord address
             , Foldable (Cell address)
             , ValueRoots address value
             )
          => Live address                      -- ^ The set of root addresses.
          -> Heap address (Cell address) value -- ^ The heap to trace addresses through.
          -> Live address                      -- ^ The set of addresses reachable from the root set.
reachable roots heap = go mempty roots
  where go seen set = case liveSplit set of
          Nothing -> seen
          Just (a, as) -> go (liveInsert a seen) (case heapLookupAll a heap of
            Just values -> liveDifference (foldr ((<>) . valueRoots) mempty values <> as) seen
            _           -> seen)


providingLiveSet :: Effectful (m address value) => m address value (Reader (Live address) ': effects) a -> m address value effects a
providingLiveSet = runReader lowerBound
