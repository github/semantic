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
collectingTerms :: ( Foldable (Cell location)
                   , Member (Reader (Live location)) effects
                   , Member (State (Heap location (Cell location) value)) effects
                   , Ord location
                   , ValueRoots location value
                   )
                => SubtermAlgebra (Base term) term (TermEvaluator term location value effects value)
                -> SubtermAlgebra (Base term) term (TermEvaluator term location value effects value)
collectingTerms recur term = do
  roots <- TermEvaluator askRoots
  v <- recur term
  v <$ TermEvaluator (modifyHeap (gc (roots <> valueRoots v)))

-- | Collect any addresses in the heap not rooted in or reachable from the given 'Live' set.
gc :: ( Ord location
      , Foldable (Cell location)
      , ValueRoots location value
      )
   => Live location                       -- ^ The set of addresses to consider rooted.
   -> Heap location (Cell location) value -- ^ A heap to collect unreachable addresses within.
   -> Heap location (Cell location) value -- ^ A garbage-collected heap.
gc roots heap = heapRestrict heap (reachable roots heap)

-- | Compute the set of addresses reachable from a given root set in a given heap.
reachable :: ( Ord location
             , Foldable (Cell location)
             , ValueRoots location value
             )
          => Live location                       -- ^ The set of root addresses.
          -> Heap location (Cell location) value -- ^ The heap to trace addresses through.
          -> Live location                       -- ^ The set of addresses reachable from the root set.
reachable roots heap = go mempty roots
  where go seen set = case liveSplit set of
          Nothing -> seen
          Just (a, as) -> go (liveInsert a seen) (case heapLookupAll a heap of
            Just values -> liveDifference (foldr ((<>) . valueRoots) mempty values <> as) seen
            _           -> seen)


providingLiveSet :: Effectful (m location value) => m location value (Reader (Live location) ': effects) a -> m location value effects a
providingLiveSet = runReader lowerBound
