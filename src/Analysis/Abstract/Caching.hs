{-# LANGUAGE GADTs, TypeOperators #-}
module Analysis.Abstract.Caching
( cachingTerms
, convergingModules
, caching
) where

import Control.Abstract.Configuration
import Control.Abstract
import Data.Abstract.Cache
import Data.Abstract.BaseError
import Data.Abstract.Environment
import Data.Abstract.Module
import Data.Abstract.Ref
import Prologue

-- | Look up the set of values for a given configuration in the in-cache.
consultOracle :: (Cacheable term address value, Member (Reader (Cache term address value)) effects)
              => Configuration term address value
              -> TermEvaluator term address value effects (Set (Cached address value))
consultOracle configuration = fromMaybe mempty . cacheLookup configuration <$> ask

-- | Run an action with the given in-cache.
withOracle :: Member (Reader (Cache term address value)) effects
           => Cache term address value
           -> TermEvaluator term address value effects a
           -> TermEvaluator term address value effects a
withOracle cache = local (const cache)


-- | Look up the set of values for a given configuration in the out-cache.
lookupCache :: (Cacheable term address value, Member (State (Cache term address value)) effects)
            => Configuration term address value
            -> TermEvaluator term address value effects (Maybe (Set (Cached address value)))
lookupCache configuration = cacheLookup configuration <$> get

-- | Run an action, caching its result and 'Heap' under the given configuration.
cachingConfiguration :: (Cacheable term address value, Member (State (Cache term address value)) effects, Member (State (Heap address address value)) effects)
                     => Configuration term address value
                     -> Set (Cached address value)
                     -> TermEvaluator term address value effects (ValueRef address)
                     -> TermEvaluator term address value effects (ValueRef address)
cachingConfiguration configuration values action = do
  modify' (cacheSet configuration values)
  result <- Cached <$> action <*> TermEvaluator getHeap
  cachedValue result <$ modify' (cacheInsert configuration result)

putCache :: Member (State (Cache term address value)) effects
         => Cache term address value
         -> TermEvaluator term address value effects ()
putCache = put

-- | Run an action starting from an empty out-cache, and return the out-cache afterwards.
isolateCache :: Member (State (Cache term address value)) effects
             => TermEvaluator term address value effects a
             -> TermEvaluator term address value effects (Cache term address value)
isolateCache action = putCache lowerBound *> action *> get


-- | Analyze a term using the in-cache as an oracle & storing the results of the analysis in the out-cache.
cachingTerms :: ( Cacheable term address value
                , Corecursive term
                , Member NonDet effects
                , Member (Reader (Cache term address value)) effects
                , Member (Reader (Live address)) effects
                , Member (State (Cache term address value)) effects
                , Member (Env address) effects
                , Member (State (Heap address address value)) effects
                )
             => SubtermAlgebra (Base term) term (TermEvaluator term address value effects (ValueRef address))
             -> SubtermAlgebra (Base term) term (TermEvaluator term address value effects (ValueRef address))
cachingTerms recur term = do
  c <- getConfiguration (embedSubterm term)
  cached <- lookupCache c
  case cached of
    Just pairs -> scatter pairs
    Nothing -> do
      pairs <- consultOracle c
      cachingConfiguration c pairs (recur term)

convergingModules :: ( AbstractValue address value effects
                     , Cacheable term address value
                     , Member Fresh effects
                     , Member NonDet effects
                     , Member (Reader (Cache term address value)) effects
                     , Member (Reader (Live address)) effects
                     , Member (Reader ModuleInfo) effects
                     , Member (Reader Span) effects
                     , Member (Resumable (BaseError (EnvironmentError address))) effects
                     , Member (State (Cache term address value)) effects
                     , Member (Env address) effects
                     , Member (State (Heap address address value)) effects
                     , Effects effects
                     )
                  => SubtermAlgebra Module term (TermEvaluator term address value effects address)
                  -> SubtermAlgebra Module term (TermEvaluator term address value effects address)
convergingModules recur m = do
  c <- getConfiguration (subterm (moduleBody m))
  -- Convergence here is predicated upon an Eq instance, not α-equivalence
  cache <- converge lowerBound (\ prevCache -> isolateCache $ do
    TermEvaluator (putHeap        (configurationHeap    c))
    TermEvaluator (putEvalContext (configurationContext c))
    -- We need to reset fresh generation so that this invocation converges.
    resetFresh 0 $
    -- This is subtle: though the calling context supports nondeterminism, we want
    -- to corral all the nondeterminism that happens in this @eval@ invocation, so
    -- that it doesn't "leak" to the calling context and diverge (otherwise this
    -- would never complete). We don’t need to use the values, so we 'gather' the
    -- nondeterministic values into @()@.
      withOracle prevCache (gatherM (const ()) (recur m)))
  TermEvaluator (address =<< runTermEvaluator (maybe empty scatter (cacheLookup c cache)))

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
scatter :: (Foldable t, Member NonDet effects, Member (State (Heap address address value)) effects) => t (Cached address value) -> TermEvaluator term address value effects (ValueRef address)
scatter = foldMapA (\ (Cached value heap') -> TermEvaluator (putHeap heap') $> value)


caching :: Effects effects => TermEvaluator term address value (NonDet ': Reader (Cache term address value) ': State (Cache term address value) ': effects) a -> TermEvaluator term address value effects (Cache term address value, [a])
caching
  = runState lowerBound
  . runReader lowerBound
  . runNonDet
