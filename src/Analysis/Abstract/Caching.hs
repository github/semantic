{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Caching
( Caching
) where

import Control.Abstract.Analysis
import Data.Abstract.Address
import Data.Abstract.Cache
import Data.Abstract.Configuration
import Data.Abstract.Heap
import Data.Abstract.Module
import Prologue

-- | The effects necessary for caching analyses.
type CachingEffects location term value effects
  = Fresh                              -- For 'MonadFresh'.
 ': NonDet                             -- For 'Alternative' and 'MonadNonDet'.
 ': Reader (Cache location term value) -- The in-cache used as an oracle while converging on a result.
 ': State  (Cache location term value) -- The out-cache used to record results in each iteration of convergence.
 ': effects

-- | A (coinductively-)cached analysis suitable for guaranteeing termination of (suitably finitized) analyses over recursive programs.
newtype Caching m (effects :: [* -> *]) a = Caching (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadControl term (m effects)                    => MonadControl term (Caching m effects)
deriving instance MonadEnvironment location value (m effects)      => MonadEnvironment location value (Caching m effects)
deriving instance MonadHeap location value (m effects)             => MonadHeap location value (Caching m effects)
deriving instance MonadModuleTable location term value (m effects) => MonadModuleTable location term value (Caching m effects)
deriving instance MonadEvaluator location term value (m effects)   => MonadEvaluator location term value (Caching m effects)

-- | Functionality used to perform caching analysis. This is not exported, and exists primarily for organizational reasons.
class MonadEvaluator location term value m => MonadCaching location term value m where
  -- | Look up the set of values for a given configuration in the in-cache.
  consultOracle :: Configuration location term value -> m (Set (value, Heap location value))
  -- | Run an action with the given in-cache.
  withOracle :: Cache location term value -> m a -> m a

  -- | Look up the set of values for a given configuration in the out-cache.
  lookupCache :: Configuration location term value -> m (Maybe (Set (value, Heap location value)))
  -- | Run an action, caching its result and 'Heap' under the given configuration.
  caching :: Configuration location term value -> Set (value, Heap location value) -> m value -> m value

  -- | Run an action starting from an empty out-cache, and return the out-cache afterwards.
  isolateCache :: m a -> m (Cache location term value)

instance ( Effectful m
         , Members (CachingEffects location term value '[]) effects
         , MonadEvaluator location term value (m effects)
         , Ord (Cell location value)
         , Ord location
         , Ord term
         , Ord value
         )
         => MonadCaching location term value (Caching m effects) where
  consultOracle configuration = raise (fromMaybe mempty . cacheLookup configuration <$> ask)
  withOracle cache = raise . local (const cache) . lower

  lookupCache configuration = raise (cacheLookup configuration <$> get)
  caching configuration values action = do
    raise (modify (cacheSet configuration values))
    result <- (,) <$> action <*> getHeap
    raise (modify (cacheInsert configuration result))
    pure (fst result)

  isolateCache action = raise (put (mempty :: Cache location term value)) *> action *> raise get

-- | This instance coinductively iterates the analysis of a term until the results converge.
instance ( Corecursive term
         , Effectful m
         , Members (CachingEffects (LocationFor value) term value '[]) effects
         , MonadAnalysis term value (m effects)
         , MonadFresh (m effects)
         , MonadNonDet (m effects)
         , Ord (CellFor value)
         , Ord (LocationFor value)
         , Ord term
         , Ord value
         )
         => MonadAnalysis term value (Caching m effects) where
  -- We require the 'CachingEffects' in addition to the underlying analysis’ 'Effects'.
  type Effects term value (Caching m effects) = CachingEffects (LocationFor value) term value (Effects term value (m effects))

  -- Analyze a term using the in-cache as an oracle & storing the results of the analysis in the out-cache.
  analyzeTerm recur e = do
    c <- getConfiguration (embedSubterm e)
    cached <- lookupCache c
    case cached of
      Just pairs -> scatter pairs
      Nothing -> do
        pairs <- consultOracle c
        caching c pairs (liftAnalyze analyzeTerm recur e)

  analyzeModule recur m = do
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
      withOracle prevCache (gather (const ()) (liftAnalyze analyzeModule recur m))) mempty
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
scatter :: (Alternative m, Foldable t, MonadEvaluator location term value m) => t (a, Heap location value) -> m a
scatter = foldMapA (\ (value, heap') -> putHeap heap' $> value)
