{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Caching
( type Caching
) where

import Control.Abstract.Analysis
import Data.Abstract.Cache
import Data.Abstract.Configuration
import Data.Abstract.Heap
import Data.Abstract.Module
import Data.Abstract.Value
import Prologue

-- | The effects necessary for caching analyses.
type CachingEffects term value effects
  = Fresh                        -- For 'MonadFresh'.
 ': NonDetEff                    -- For 'Alternative' and 'MonadNonDet'.
 ': Reader (CacheFor term value) -- The in-cache used as an oracle while converging on a result.
 ': State  (CacheFor term value) -- The out-cache used to record results in each iteration of convergence.
 ': effects

-- | The cache for term and abstract value types.
type CacheFor term value = Cache (LocationFor value) term value

-- | A (coinductively-)cached analysis suitable for guaranteeing termination of (suitably finitized) analyses over recursive programs.
newtype Caching m term value (effects :: [* -> *]) a = Caching (m term value effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadControl term (m term value effects) => MonadControl term (Caching m term value effects)
deriving instance MonadEnvironment value (m term value effects) => MonadEnvironment value (Caching m term value effects)
deriving instance MonadHeap value (m term value effects) => MonadHeap value (Caching m term value effects)
deriving instance MonadModuleTable term value (m term value effects) => MonadModuleTable term value (Caching m term value effects)
deriving instance MonadEvaluator term value (m term value effects) => MonadEvaluator term value (Caching m term value effects)

-- | Functionality used to perform caching analysis. This is not exported, and exists primarily for organizational reasons.
class MonadEvaluator term value m => MonadCaching term value m where
  -- | Look up the set of values for a given configuration in the in-cache.
  consultOracle :: ConfigurationFor term value -> m (Set (value, HeapFor value))
  -- | Run an action with the given in-cache.
  withOracle :: CacheFor term value -> m a -> m a

  -- | Look up the set of values for a given configuration in the out-cache.
  lookupCache :: ConfigurationFor term value -> m (Maybe (Set (value, HeapFor value)))
  -- | Run an action, caching its result and 'Heap' under the given configuration.
  caching :: ConfigurationFor term value -> Set (value, HeapFor value) -> m value -> m value

  -- | Run an action starting from an empty out-cache, and return the out-cache afterwards.
  isolateCache :: m a -> m (CacheFor term value)

instance ( Effectful (m term value)
         , Members (CachingEffects term value '[]) effects
         , MonadEvaluator term value (m term value effects)
         , Ord (CellFor value)
         , Ord (LocationFor value)
         , Ord term
         , Ord value
         )
         => MonadCaching term value (Caching m term value effects) where
  consultOracle configuration = raise (fromMaybe mempty . cacheLookup configuration <$> ask)
  withOracle cache = raise . local (const cache) . lower

  lookupCache configuration = raise (cacheLookup configuration <$> get)
  caching configuration values action = do
    raise (modify (cacheSet configuration values))
    result <- (,) <$> action <*> getHeap
    raise (modify (cacheInsert configuration result))
    pure (fst result)

  isolateCache action = raise (put (mempty :: CacheFor term value)) *> action *> raise get

-- | This instance coinductively iterates the analysis of a term until the results converge.
instance ( Corecursive term
         , Effectful (m term value)
         , MonadAnalysis term value (m term value effects)
         , MonadFresh (m term value effects)
         , MonadNonDet (m term value effects)
         , Members (CachingEffects term value '[]) effects
         , Ord (CellFor value)
         , Ord (LocationFor value)
         , Ord term
         , Ord value
         )
         => MonadAnalysis term value (Caching m term value effects) where
  -- We require the 'CachingEffects' in addition to the underlying analysis’ 'RequiredEffects'.
  type RequiredEffects term value (Caching m term value effects) = CachingEffects term value (RequiredEffects term value (m term value effects))

  -- Analyze a term using the in-cache as an oracle & storing the results of the analysis in the out-cache.
  analyzeTerm e = do
    c <- getConfiguration (embedSubterm e)
    cached <- lookupCache c
    case cached of
      Just pairs -> scatter pairs
      Nothing -> do
        pairs <- consultOracle c
        caching c pairs (liftAnalyze analyzeTerm e)

  analyzeModule m = do
    c <- getConfiguration (subterm (moduleBody m))
    -- Convergence here is predicated upon an Eq instance, not α-equivalence
    cache <- converge (\ prevCache -> isolateCache $ do
      putHeap (configurationHeap c)
      -- We need to reset fresh generation so that this invocation converges.
      reset 0
      -- This is subtle: though the calling context supports nondeterminism, we want
      -- to corral all the nondeterminism that happens in this @eval@ invocation, so
      -- that it doesn't "leak" to the calling context and diverge (otherwise this
      -- would never complete). We don’t need to use the values, so we 'gather' the
      -- nondeterministic values into @()@.
      withOracle prevCache (gather (const ()) (liftAnalyze analyzeModule m))) mempty
    maybe empty scatter (cacheLookup c cache)

-- | Iterate a monadic action starting from some initial seed until the results converge.
--
--   This applies the Kleene fixed-point theorem to finitize a monotone action. cf https://en.wikipedia.org/wiki/Kleene_fixed-point_theorem
converge :: (Eq a, Monad m)
         => (a -> m a) -- ^ A monadic action to perform at each iteration, starting from the result of the previous iteration or from the seed value for the first iteration.
         -> a          -- ^ An initial seed value to iterate from.
         -> m a        -- ^ A computation producing the least fixed point (the first value at which the actions converge).
converge f = loop
  where loop x = do
          x' <- f x
          if x' == x then
            pure x
          else
            loop x'

-- | Nondeterministically write each of a collection of stores & return their associated results.
scatter :: (Alternative m, Foldable t, MonadEvaluator term value m) => t (a, Heap (LocationFor value) value) -> m a
scatter = foldMapA (\ (value, heap') -> putHeap heap' $> value)
