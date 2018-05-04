{-# LANGUAGE TypeOperators #-}
module Analysis.Abstract.Collecting
( collectingTerms
, providingLiveSet
) where

import Control.Abstract.Evaluator
import Control.Abstract.Value
import Data.Abstract.Address
import Data.Abstract.Heap
import Data.Abstract.Live
import Data.Semilattice.Lower
import Prologue

-- | An analysis performing GC after every instruction.
collectingTerms :: ( Foldable (Cell location)
                   , Members '[ Reader (Live location value)
                              , State (Heap location value)
                              ] effects
                   , Ord location
                   , ValueRoots location value
                   )
                => SubtermAlgebra (Base term) term (Evaluator location term value effects value)
                -> SubtermAlgebra (Base term) term (Evaluator location term value effects value)
collectingTerms recur term = do
  roots <- askRoots
  v <- recur term
  v <$ modifyHeap (gc (roots <> valueRoots v))

-- | Collect any addresses in the heap not rooted in or reachable from the given 'Live' set.
gc :: ( Ord location
      , Foldable (Cell location)
      , ValueRoots location value
      )
   => Live location value -- ^ The set of addresses to consider rooted.
   -> Heap location value -- ^ A heap to collect unreachable addresses within.
   -> Heap location value -- ^ A garbage-collected heap.
gc roots heap = heapRestrict heap (reachable roots heap)

-- | Compute the set of addresses reachable from a given root set in a given heap.
reachable :: ( Ord location
             , Foldable (Cell location)
             , ValueRoots location value
             )
          => Live location value -- ^ The set of root addresses.
          -> Heap location value -- ^ The heap to trace addresses through.
          -> Live location value -- ^ The set of addresses reachable from the root set.
reachable roots heap = go mempty roots
  where go seen set = case liveSplit set of
          Nothing -> seen
          Just (a, as) -> go (liveInsert a seen) (case heapLookupAll a heap of
            Just values -> liveDifference (foldr ((<>) . valueRoots) mempty values <> as) seen
            _           -> seen)


providingLiveSet :: Evaluator location term value (Reader (Live location value) ': effects) a -> Evaluator location term value effects a
providingLiveSet = handleReader lowerBound
