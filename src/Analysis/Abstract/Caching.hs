{-# LANGUAGE GADTs, TypeOperators #-}
module Analysis.Abstract.Caching
( cachingTerms
, convergingModules
, caching
) where

import Control.Abstract
import Data.Abstract.Cache
import Data.Abstract.Module
import Data.Semilattice.Lower
import Prologue

type InCache  term location value = Reader (Cache term location (Cell location) value)
type OutCache term location value = State  (Cache term location (Cell location) value)

-- | Look up the set of values for a given configuration in the in-cache.
consultOracle :: (Cacheable term location (Cell location) value, Member (InCache term location value) effects)
              => Configuration term location (Cell location) value
              -> TermEvaluator term location value effects (Set (Cached location (Cell location) value))
consultOracle configuration = fromMaybe mempty . cacheLookup configuration <$> ask

-- | Run an action with the given in-cache.
withOracle :: Member (InCache term location value) effects
           => Cache term location (Cell location) value
           -> TermEvaluator term location value effects a
           -> TermEvaluator term location value effects a
withOracle cache = local (const cache)


-- | Look up the set of values for a given configuration in the out-cache.
lookupCache :: (Cacheable term location (Cell location) value, Member (OutCache term location value) effects)
            => Configuration term location (Cell location) value
            -> TermEvaluator term location value effects (Maybe (Set (Cached location (Cell location) value)))
lookupCache configuration = cacheLookup configuration <$> get

-- | Run an action, caching its result and 'Heap' under the given configuration.
cachingConfiguration :: (Cacheable term location (Cell location) value, Members '[OutCache term location value, State (Heap location (Cell location) value)] effects)
                     => Configuration term location (Cell location) value
                     -> Set (Cached location (Cell location) value)
                     -> TermEvaluator term location value effects value
                     -> TermEvaluator term location value effects value
cachingConfiguration configuration values action = do
  modify' (cacheSet configuration values)
  result <- Cached <$> action <*> TermEvaluator getHeap
  cachedValue result <$ modify' (cacheInsert configuration result)

putCache :: Member (OutCache term location value) effects
         => Cache term location (Cell location) value
         -> TermEvaluator term location value effects ()
putCache = put

-- | Run an action starting from an empty out-cache, and return the out-cache afterwards.
isolateCache :: Member (OutCache term location value) effects
             => TermEvaluator term location value effects a
             -> TermEvaluator term location value effects (Cache term location (Cell location) value)
isolateCache action = putCache lowerBound *> action *> get


-- | Analyze a term using the in-cache as an oracle & storing the results of the analysis in the out-cache.
cachingTerms :: ( Cacheable term location (Cell location) value
                , Corecursive term
                , Members '[ Fresh
                           , InCache term location value
                           , NonDet
                           , OutCache term location value
                           , Reader (Live location value)
                           , State (Environment location value)
                           , State (Heap location (Cell location) value)
                           ] effects
                )
             => SubtermAlgebra (Base term) term (TermEvaluator term location value effects value)
             -> SubtermAlgebra (Base term) term (TermEvaluator term location value effects value)
cachingTerms recur term = do
  c <- getConfiguration (embedSubterm term)
  cached <- lookupCache c
  case cached of
    Just pairs -> scatter pairs
    Nothing -> do
      pairs <- consultOracle c
      cachingConfiguration c pairs (recur term)

convergingModules :: ( Cacheable term location (Cell location) value
                     , Members '[ Fresh
                                , InCache term location value
                                , NonDet
                                , OutCache term location value
                                , Reader (Live location value)
                                , State (Environment location value)
                                , State (Heap location (Cell location) value)
                                ] effects
                     )
                  => SubtermAlgebra Module term (TermEvaluator term location value effects value)
                  -> SubtermAlgebra Module term (TermEvaluator term location value effects value)
convergingModules recur m = do
  c <- getConfiguration (subterm (moduleBody m))
  -- Convergence here is predicated upon an Eq instance, not α-equivalence
  cache <- converge lowerBound (\ prevCache -> isolateCache $ do
    TermEvaluator (putEnv  (configurationEnvironment c))
    TermEvaluator (putHeap (configurationHeap        c))
    -- We need to reset fresh generation so that this invocation converges.
    resetFresh 0 $
    -- This is subtle: though the calling context supports nondeterminism, we want
    -- to corral all the nondeterminism that happens in this @eval@ invocation, so
    -- that it doesn't "leak" to the calling context and diverge (otherwise this
    -- would never complete). We don’t need to use the values, so we 'gather' the
    -- nondeterministic values into @()@.
      withOracle prevCache (gatherM (const ()) (recur m)))
  maybe empty scatter (cacheLookup c cache)


-- | Iterate a monadic action starting from some initial seed until the results converge.
--
--   This applies the Kleene fixed-point theorem to finitize a monotone action. cf https://en.wikipedia.org/wiki/Kleene_fixed-point_theorem
converge :: (Eq a, Monad m)
         => a          -- ^ An initial seed value to iterate from.
         -> (a -> m a) -- ^ A monadic action to perform at each iteration, starting from the result of the previous iteration or from the seed value for the first iteration.
         -> m a        -- ^ A computation producing the least fixed point (the first value at which the actions converge).
converge seed f = loop seed
  where loop x = do
          x' <- f x
          if x' == x then
            pure x
          else
            loop x'

-- | Nondeterministically write each of a collection of stores & return their associated results.
scatter :: (Foldable t, Members '[NonDet, State (Heap location (Cell location) value)] effects) => t (Cached location (Cell location) value) -> TermEvaluator term location value effects value
scatter = foldMapA (\ (Cached value heap') -> TermEvaluator (putHeap heap') $> value)


caching :: Alternative f => TermEvaluator term location value (NonDet ': InCache term location value ': OutCache term location value ': effects) a -> TermEvaluator term location value effects (f a, Cache term location (Cell location) value)
caching
  = runState lowerBound
  . runReader lowerBound
  . runNonDetA
