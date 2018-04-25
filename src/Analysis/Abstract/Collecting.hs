{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For the Interpreter instance’s MonadEvaluator constraint
module Analysis.Abstract.Collecting
( Collecting
) where

import Control.Abstract.Analysis
import Data.Abstract.Address
import Data.Abstract.Configuration
import Data.Abstract.Heap
import Data.Abstract.Live
import Prologue

newtype Collecting m (effects :: [* -> *]) a = Collecting { runCollecting :: m effects a }
  deriving (Alternative, Applicative, Effectful, Functor, Monad)

instance ( Effectful m
         , Member (Reader (Live location value)) effects
         , MonadEvaluator location term value effects m
         )
      => MonadEvaluator location term value effects (Collecting m) where
  getConfiguration term = Configuration term <$> askRoots <*> getEnv <*> getHeap


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


-- | Retrieve the local 'Live' set.
askRoots :: (Effectful m, Member (Reader (Live location value)) effects) => m effects (Live location value)
askRoots = raise ask

-- | Run a computation with the given 'Live' set added to the local root set.
-- extraRoots :: (Effectful m, Member (Reader (Live location value)) effects, Ord location) => Live location value -> m effects a -> m effects a
-- extraRoots roots = raise . local (<> roots) . lower


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


instance ( Interpreter effects result rest m
         , MonadEvaluator location term value effects m
         , Ord location
         )
      => Interpreter (Reader (Live location value) ': effects) result rest (Collecting m) where
  interpret = interpret . runCollecting . raiseHandler (`runReader` mempty)
