{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Collecting
( type Collecting
) where

import Control.Abstract.Analysis
import Data.Abstract.Address
import Data.Abstract.Configuration
import Data.Abstract.Heap
import Data.Abstract.Live
import Data.Abstract.Value
import Prologue

newtype Collecting m term value (effects :: [* -> *]) a = Collecting (m term value effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadControl term (m term value effects) => MonadControl term (Collecting m term value effects)
deriving instance MonadEnvironment value (m term value effects) => MonadEnvironment value (Collecting m term value effects)
deriving instance MonadHeap value (m term value effects) => MonadHeap value (Collecting m term value effects)
deriving instance MonadModuleTable term value (m term value effects) => MonadModuleTable term value (Collecting m term value effects)

instance ( Effectful (m term value)
         , Member (Reader (Live (LocationFor value) value)) effects
         , MonadEvaluator term value (m term value effects)
         )
         => MonadEvaluator term value (Collecting m term value effects) where
  getConfiguration term = Configuration term <$> askRoots <*> askLocalEnv <*> getHeap


instance ( Effectful (m term value)
         , Foldable (Cell (LocationFor value))
         , Member (Reader (Live (LocationFor value) value)) effects
         , MonadAnalysis term value (m term value effects)
         , Ord (LocationFor value)
         , ValueRoots value
         )
         => MonadAnalysis term value (Collecting m term value effects) where
  type RequiredEffects term value (Collecting m term value effects)
    = Reader (Live (LocationFor value) value)
   ': RequiredEffects term value (m term value effects)

  -- Small-step evaluation which garbage-collects any non-rooted addresses after evaluating each term.
  analyzeTerm term = do
    roots <- askRoots
    v <- liftAnalyze analyzeTerm term
    modifyHeap (gc (roots <> valueRoots v))
    pure v


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
