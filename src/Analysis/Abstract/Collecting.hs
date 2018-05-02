{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For the Interpreter instance’s Evaluator constraint
module Analysis.Abstract.Collecting
( Collecting
, Retaining
) where

import Control.Abstract.Analysis
import Data.Abstract.Address
import Data.Abstract.Heap
import Data.Abstract.Live
import Prologue

-- | An analysis performing GC after every instruction.
newtype Collecting m (effects :: [* -> *]) a = Collecting { runCollecting :: m effects a }
  deriving (Alternative, Applicative, Effectful, Functor, Monad)

deriving instance Evaluator location term value m => Evaluator location term value (Collecting m)


instance ( Effectful m
         , Foldable (Cell location)
         , Member (Reader (Live location value)) effects
         , MonadAnalysis location term value effects m
         , Ord location
         , ValueRoots location value
         )
      => MonadAnalysis location term value effects (Collecting m) where
  -- Small-step evaluation which garbage-collects any non-rooted addresses after evaluating each term.
  analyzeTerm recur term = do
    roots <- askRoots
    v <- liftAnalyze analyzeTerm recur term
    modifyHeap (gc (roots <> valueRoots v))
    pure v

  analyzeModule = liftAnalyze analyzeModule


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


instance ( Evaluator location term value m
         , Interpreter m effects
         , Ord location
         )
      => Interpreter (Collecting m) (Reader (Live location value) ': effects) where
  type Result (Collecting m) (Reader (Live location value) ': effects) result = Result m effects result
  interpret = interpret . runCollecting . raiseHandler (`runReader` mempty)


-- | An analysis providing a 'Live' set, but never performing GC.
newtype Retaining m (effects :: [* -> *]) a = Retaining { runRetaining :: m effects a }
  deriving (Alternative, Applicative, Effectful, Functor, Monad)

deriving instance Evaluator location term value m => Evaluator location term value (Retaining m)
deriving instance MonadAnalysis location term value effects m => MonadAnalysis location term value effects (Retaining m)

instance ( Evaluator location term value m
         , Interpreter m effects
         , Ord location
         )
      => Interpreter (Retaining m) (Reader (Live location value) ': effects) where
  type Result (Retaining m) (Reader (Live location value) ': effects) result = Result m effects result
  interpret = interpret . runRetaining . raiseHandler (`runReader` mempty)
