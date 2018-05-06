{-# LANGUAGE GADTs, TypeOperators #-}
module Analysis.Abstract.Caching
( cachingTerms
, convergingModules
, caching
) where

import Control.Abstract.Evaluator
import Control.Monad.Effect hiding (interpret)
import Data.Abstract.Cache
import Data.Abstract.Configuration
import Data.Abstract.Live
import Data.Abstract.Module
import Data.Semilattice.Lower
import Prologue

-- | Look up the set of values for a given configuration in the in-cache.
consultOracle :: (Cacheable location term value, Member (Reader (Cache location term value)) effects) => Configuration location term value -> Evaluator location term value effects (Set (value, Heap location value))
consultOracle configuration = raise (fromMaybe mempty . cacheLookup configuration <$> ask)

-- | Run an action with the given in-cache.
withOracle :: Member (Reader (Cache location term value)) effects => Cache location term value -> Evaluator location term value effects a -> Evaluator location term value effects a
withOracle cache = raiseHandler (local (const cache))


-- | Look up the set of values for a given configuration in the out-cache.
lookupCache :: (Cacheable location term value, Member (State (Cache location term value)) effects) => Configuration location term value -> Evaluator location term value effects (Maybe (Set (value, Heap location value)))
lookupCache configuration = raise (cacheLookup configuration <$> get)

-- | Run an action, caching its result and 'Heap' under the given configuration.
cachingConfiguration :: (Cacheable location term value, Member (State (Cache location term value)) effects, Member (State (Heap location value)) effects, Monad (Evaluator location term value effects)) => Configuration location term value -> Set (value, Heap location value) -> Evaluator location term value effects value -> Evaluator location term value effects value
cachingConfiguration configuration values action = do
  raise (modify (cacheSet configuration values))
  result <- (,) <$> action <*> raise get
  raise (modify (cacheInsert configuration result))
  pure (fst result)

putCache :: (Member (State (Cache location term value)) effects) => Cache location term value -> Evaluator location term value effects ()
putCache = raise . put

-- | Run an action starting from an empty out-cache, and return the out-cache afterwards.
isolateCache :: Member (State (Cache location term value)) effects => Evaluator location term value effects a -> Evaluator location term value effects (Cache location term value)
isolateCache action = putCache lowerBound *> action *> raise get


-- | Analyze a term using the in-cache as an oracle & storing the results of the analysis in the out-cache.
cachingTerms :: ( Cacheable location term value
                , Corecursive term
                , Members '[ Fresh
                           , NonDet
                           , Reader (Cache location term value)
                           , Reader (Live location value)
                           , State (Cache location term value)
                           , State (Environment location value)
                           , State (Heap location value)
                           ] effects
                )
             => SubtermAlgebra (Base term) term (Evaluator location term value effects value)
             -> SubtermAlgebra (Base term) term (Evaluator location term value effects value)
cachingTerms recur term = do
  c <- getConfiguration (embedSubterm term)
  cached <- lookupCache c
  case cached of
    Just pairs -> scatter pairs
    Nothing -> do
      pairs <- consultOracle c
      cachingConfiguration c pairs (recur term)

convergingModules :: ( Cacheable location term value
                     , Members '[ Fresh
                                , NonDet
                                , Reader (Cache location term value)
                                , Reader (Live location value)
                                , State (Cache location term value)
                                , State (Environment location value)
                                , State (Heap location value)
                                ] effects
                     )
                  => SubtermAlgebra Module term (Evaluator location term value effects value)
                  -> SubtermAlgebra Module term (Evaluator location term value effects value)
convergingModules recur m = do
  c <- getConfiguration (subterm (moduleBody m))
  -- Convergence here is predicated upon an Eq instance, not α-equivalence
  cache <- converge (\ prevCache -> isolateCache $ do
    putHeap (configurationHeap c)
    -- We need to reset fresh generation so that this invocation converges.
    reset 0 $
    -- This is subtle: though the calling context supports nondeterminism, we want
    -- to corral all the nondeterminism that happens in this @eval@ invocation, so
    -- that it doesn't "leak" to the calling context and diverge (otherwise this
    -- would never complete). We don’t need to use the values, so we 'gather' the
    -- nondeterministic values into @()@.
      withOracle prevCache (raiseHandler (gather (const ())) (recur m))) lowerBound
  maybe empty scatter (cacheLookup c cache)


reset :: (Effectful m, Member Fresh effects) => Int -> m effects a -> m effects a
reset start = raiseHandler (interposeState start (const pure) (\ counter Fresh yield -> (yield $! succ counter) counter))

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
scatter :: (Foldable t, Members '[NonDet, State (Heap location value)] effects) => t (a, Heap location value) -> Evaluator location term value effects a
scatter = foldMapA (\ (value, heap') -> putHeap heap' $> value)


caching :: Alternative f => Evaluator location term value (NonDet ': Reader (Cache location term value) ': State (Cache location term value) ': effects) a -> Evaluator location term value effects (f a, Cache location term value)
caching
  = handleState lowerBound
  . runReader lowerBound
  . raiseHandler makeChoiceA
