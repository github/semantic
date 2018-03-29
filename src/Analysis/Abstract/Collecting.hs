{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Collecting
( Collecting
) where

import Control.Abstract.Analysis
import Data.Abstract.Address
import Data.Abstract.Configuration
import Data.Abstract.Heap
import Data.Abstract.Live
import Prologue

newtype Collecting m (effects :: [* -> *]) a = Collecting (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadControl term (m effects)           => MonadControl term (Collecting m effects)
deriving instance MonadEnvironment value (m effects)      => MonadEnvironment value (Collecting m effects)
deriving instance MonadHeap value (m effects)             => MonadHeap value (Collecting m effects)
deriving instance MonadModuleTable term value (m effects) => MonadModuleTable term value (Collecting m effects)

instance ( Effectful m
         , Member (Reader (Live (LocationFor value) value)) effects
         , MonadEvaluator term value (m effects)
         )
      => MonadEvaluator term value (Collecting m effects) where
  getConfiguration term = Configuration term <$> askRoots <*> getEnv <*> getHeap

  askModuleStack = Collecting askModuleStack


instance ( Effectful m
         , Foldable (Cell (LocationFor value))
         , Member (Reader (Live (LocationFor value) value)) effects
         , MonadAnalysis term value (m effects)
         , Ord (LocationFor value)
         , ValueRoots value
         )
      => MonadAnalysis term value (Collecting m effects) where
  type Effects term value (Collecting m effects)
    = Reader (Live (LocationFor value) value)
   ': Effects term value (m effects)

  -- Small-step evaluation which garbage-collects any non-rooted addresses after evaluating each term.
  analyzeTerm recur term = do
    roots <- askRoots
    v <- liftAnalyze analyzeTerm recur term
    modifyHeap (gc (roots <> valueRoots v))
    pure v

  analyzeModule = liftAnalyze analyzeModule


-- | Retrieve the local 'Live' set.
askRoots :: (Effectful m, Member (Reader (Live (LocationFor value) value)) effects) => m effects (Live (LocationFor value) value)
askRoots = raise ask

-- | Run a computation with the given 'Live' set added to the local root set.
-- extraRoots :: (Effectful m, Member (Reader (Live (LocationFor value) value)) effects, Ord (LocationFor value)) => Live (LocationFor value) value -> m effects a -> m effects a
-- extraRoots roots = raise . local (<> roots) . lower


-- | Collect any addresses in the heap not rooted in or reachable from the given 'Live' set.
gc :: ( Ord (LocationFor value)
      , Foldable (Cell (LocationFor value))
      , ValueRoots value
      )
   => LiveFor value -- ^ The set of addresses to consider rooted.
   -> HeapFor value -- ^ A heap to collect unreachable addresses within.
   -> HeapFor value -- ^ A garbage-collected heap.
gc roots heap = heapRestrict heap (reachable roots heap)

-- | Compute the set of addresses reachable from a given root set in a given heap.
reachable :: ( Ord (LocationFor value)
             , Foldable (Cell (LocationFor value))
             , ValueRoots value
             )
          => LiveFor value -- ^ The set of root addresses.
          -> HeapFor value -- ^ The heap to trace addresses through.
          -> LiveFor value -- ^ The set of addresses reachable from the root set.
reachable roots heap = go mempty roots
  where go seen set = case liveSplit set of
          Nothing -> seen
          Just (a, as) -> go (liveInsert a seen) (case heapLookupAll a heap of
            Just values -> liveDifference (foldr ((<>) . valueRoots) mempty values <> as) seen
            _           -> seen)
