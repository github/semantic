{-# LANGUAGE ConstraintKinds, GADTs, GeneralizedNewtypeDeriving, TypeOperators #-}
module Analysis.Abstract.Caching.FlowSensitive
( cachingTerms
, convergingModules
, caching
) where

import Control.Abstract
import Data.Abstract.BaseError
import Data.Abstract.Environment
import Data.Abstract.Module
import Data.Abstract.Ref
import Data.Map.Monoidal as Monoidal
import Prologue

-- | Look up the set of values for a given configuration in the in-cache.
consultOracle :: (Cacheable term address value, Member (Reader (Cache term address value)) effects)
              => Configuration term address value
              -> Evaluator term address value effects (Set (Cached address value))
consultOracle configuration = fromMaybe mempty . cacheLookup configuration <$> ask

-- | Run an action with the given in-cache.
withOracle :: Member (Reader (Cache term address value)) effects
           => Cache term address value
           -> Evaluator term address value effects a
           -> Evaluator term address value effects a
withOracle cache = local (const cache)


-- | Look up the set of values for a given configuration in the out-cache.
lookupCache :: (Cacheable term address value, Member (State (Cache term address value)) effects)
            => Configuration term address value
            -> Evaluator term address value effects (Maybe (Set (Cached address value)))
lookupCache configuration = cacheLookup configuration <$> get

-- | Run an action, caching its result and 'Heap' under the given configuration.
cachingConfiguration :: (Cacheable term address value, Member (State (Cache term address value)) effects, Member (State (Heap address value)) effects)
                     => Configuration term address value
                     -> Set (Cached address value)
                     -> Evaluator term address value effects (ValueRef address)
                     -> Evaluator term address value effects (ValueRef address)
cachingConfiguration configuration values action = do
  modify' (cacheSet configuration values)
  result <- Cached <$> action <*> getHeap
  cachedValue result <$ modify' (cacheInsert configuration result)

putCache :: Member (State (Cache term address value)) effects
         => Cache term address value
         -> Evaluator term address value effects ()
putCache = put

-- | Run an action starting from an empty out-cache, and return the out-cache afterwards.
isolateCache :: Member (State (Cache term address value)) effects
             => Evaluator term address value effects a
             -> Evaluator term address value effects (Cache term address value)
isolateCache action = putCache lowerBound *> action *> get


-- | Analyze a term using the in-cache as an oracle & storing the results of the analysis in the out-cache.
cachingTerms :: ( Cacheable term address value
                , Corecursive term
                , Member NonDet effects
                , Member (Reader (Cache term address value)) effects
                , Member (Reader (Live address)) effects
                , Member (State (Cache term address value)) effects
                , Member (Env address) effects
                , Member (State (Heap address value)) effects
                )
             => SubtermAlgebra (Base term) term (Evaluator term address value effects (ValueRef address))
             -> SubtermAlgebra (Base term) term (Evaluator term address value effects (ValueRef address))
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
                     , Member (State (Heap address value)) effects
                     , Effects effects
                     )
                  => Open (Module term -> Evaluator term address value effects address)
convergingModules recur m = do
  c <- getConfiguration (moduleBody m)
  -- Convergence here is predicated upon an Eq instance, not α-equivalence
  cache <- converge lowerBound (\ prevCache -> isolateCache $ do
    putHeap        (configurationHeap    c)
    putEvalContext (configurationContext c)
    -- We need to reset fresh generation so that this invocation converges.
    resetFresh 0 $
    -- This is subtle: though the calling context supports nondeterminism, we want
    -- to corral all the nondeterminism that happens in this @eval@ invocation, so
    -- that it doesn't "leak" to the calling context and diverge (otherwise this
    -- would never complete). We don’t need to use the values, so we 'gather' the
    -- nondeterministic values into @()@.
      withOracle prevCache (gatherM (const ()) (recur m)))
  address =<< maybe empty scatter (cacheLookup c cache)

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
scatter :: (Foldable t, Member NonDet effects, Member (State (Heap address value)) effects) => t (Cached address value) -> Evaluator term address value effects (ValueRef address)
scatter = foldMapA (\ (Cached value heap') -> putHeap heap' $> value)

-- | Get the current 'Configuration' with a passed-in term.
getConfiguration :: (Member (Reader (Live address)) effects, Member (Env address) effects, Member (State (Heap address value)) effects)
                 => term
                 -> Evaluator term address value effects (Configuration term address value)
getConfiguration term = Configuration term <$> askRoots <*> getEvalContext <*> getHeap


caching :: Effects effects => Evaluator term address value (NonDet ': Reader (Cache term address value) ': State (Cache term address value) ': effects) a -> Evaluator term address value effects (Cache term address value, [a])
caching
  = runState lowerBound
  . runReader lowerBound
  . runNonDet


-- | A map of 'Configuration's to 'Set's of resulting values & 'Heap's.
newtype Cache term address value = Cache { unCache :: Monoidal.Map (Configuration term address value) (Set (Cached address value)) }
  deriving (Eq, Lower, Monoid, Ord, Reducer (Configuration term address value, Cached address value), Semigroup)

-- | A single point in a program’s execution.
data Configuration term address value = Configuration
  { configurationTerm    :: term                -- ^ The “instruction,” i.e. the current term to evaluate.
  , configurationRoots   :: Live address        -- ^ The set of rooted addresses.
  , configurationContext :: EvalContext address -- ^ The evaluation context in 'configurationTerm'.
  , configurationHeap    :: Heap address value  -- ^ The heap of values.
  }
  deriving (Eq, Ord, Show)

data Cached address value = Cached
  { cachedValue :: ValueRef address
  , cachedHeap  :: Heap address value
  }
  deriving (Eq, Ord, Show)


type Cacheable term address value = (Ord address, Ord term, Ord value)

-- | Look up the resulting value & 'Heap' for a given 'Configuration'.
cacheLookup :: Cacheable term address value => Configuration term address value -> Cache term address value -> Maybe (Set (Cached address value))
cacheLookup key = Monoidal.lookup key . unCache

-- | Set the resulting value & 'Heap' for a given 'Configuration', overwriting any previous entry.
cacheSet :: Cacheable term address value => Configuration term address value -> Set (Cached address value) -> Cache term address value -> Cache term address value
cacheSet key value = Cache . Monoidal.insert key value . unCache

-- | Insert the resulting value & 'Heap' for a given 'Configuration', appending onto any previous entry.
cacheInsert :: Cacheable term address value => Configuration term address value -> Cached address value -> Cache term address value -> Cache term address value
cacheInsert = curry cons

instance (Show term, Show address, Show value) => Show (Cache term address value) where
  showsPrec d = showsUnaryWith showsPrec "Cache" d . map (second toList) . Monoidal.pairs . unCache
