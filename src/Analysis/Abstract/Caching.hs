{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For the Interpreter instance’s Evaluator constraint
module Analysis.Abstract.Caching
( Caching
) where

import Control.Abstract.Analysis
import Control.Monad.Effect hiding (interpret)
import Data.Abstract.Address
import Data.Abstract.Cache
import Data.Abstract.Configuration
import Data.Abstract.Live
import Data.Abstract.Module
import Prologue

-- | A (coinductively-)cached analysis suitable for guaranteeing termination of (suitably finitized) analyses over recursive programs.
newtype Caching m (effects :: [* -> *]) a = Caching { runCaching :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (Caching m)
deriving instance Evaluator location term value m => Evaluator location term value (Caching m)

-- | Functionality used to perform caching analysis. This is not exported, and exists primarily for organizational reasons.
class MonadEvaluator location term value effects m => MonadCaching location term value effects m where
  -- | Look up the set of values for a given configuration in the in-cache.
  consultOracle :: Configuration location term value -> m effects (Set (value, Heap location value))
  -- | Run an action with the given in-cache.
  withOracle :: Cache location term value -> m effects a -> m effects a

  -- | Look up the set of values for a given configuration in the out-cache.
  lookupCache :: Configuration location term value -> m effects (Maybe (Set (value, Heap location value)))
  -- | Run an action, caching its result and 'Heap' under the given configuration.
  caching :: Configuration location term value -> Set (value, Heap location value) -> m effects value -> m effects value

  -- | Run an action starting from an empty out-cache, and return the out-cache afterwards.
  isolateCache :: m effects a -> m effects (Cache location term value)

instance ( Effectful m
         , Member (Reader (Cache location term value)) effects
         , Member (State  (Cache location term value)) effects
         , MonadEvaluator location term value effects m
         , Ord (Cell location value)
         , Ord location
         , Ord term
         , Ord value
         )
         => MonadCaching location term value effects (Caching m) where
  consultOracle configuration = raise (fromMaybe mempty . cacheLookup configuration <$> ask)
  withOracle cache = raiseHandler (local (const cache))

  lookupCache configuration = raise (cacheLookup configuration <$> get)
  caching configuration values action = do
    raise (modify (cacheSet configuration values))
    result <- (,) <$> action <*> getHeap
    raise (modify (cacheInsert configuration result))
    pure (fst result)

  isolateCache action = raise (put (mempty :: Cache location term value)) *> action *> raise get

-- | This instance coinductively iterates the analysis of a term until the results converge.
instance ( Alternative (m effects)
         , Corecursive term
         , Effectful m
         , Member Fresh effects
         , Member NonDet effects
         , Member (Reader (Cache location term value)) effects
         , Member (Reader (Live location value)) effects
         , Member (State  (Cache location term value)) effects
         , MonadAnalysis location term value effects m
         , Ord (Cell location value)
         , Ord location
         , Ord term
         , Ord value
         )
      => MonadAnalysis location term value effects (Caching m) where
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
      reset 0 $
      -- This is subtle: though the calling context supports nondeterminism, we want
      -- to corral all the nondeterminism that happens in this @eval@ invocation, so
      -- that it doesn't "leak" to the calling context and diverge (otherwise this
      -- would never complete). We don’t need to use the values, so we 'gather' the
      -- nondeterministic values into @()@.
        withOracle prevCache (raiseHandler (gather (const ())) (liftAnalyze analyzeModule recur m))) mempty
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
scatter :: (Alternative (m effects), Evaluator location term value m, Foldable t, Member (State (Heap location value)) effects) => t (a, Heap location value) -> m effects a
scatter = foldMapA (\ (value, heap') -> putHeap heap' $> value)


instance ( Interpreter m effects
         , Evaluator location term value m
         , Ord (Cell location value)
         , Ord location
         , Ord term
         , Ord value
         )
      => Interpreter (Caching m) (NonDet ': Reader (Cache location term value) ': State (Cache location term value) ': effects) where
  type Result (Caching m) (NonDet ': Reader (Cache location term value) ': State (Cache location term value) ': effects) result = Result m effects ([result], Cache location term value)
  interpret
    = interpret
    . runCaching
    . raiseHandler
      ( flip runState mempty
      . flip runReader mempty
      . makeChoiceA @[])
