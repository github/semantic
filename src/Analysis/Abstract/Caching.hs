{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Caching
  -- ( evaluateCache )
  where

import Control.Abstract.Analysis
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.NonDet
import Data.Abstract.Address
import Data.Abstract.Cache
import Data.Abstract.Configuration
import Data.Abstract.Evaluatable
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Monoid (Alt(..))
import Prologue

-- | The effects necessary for caching analyses.
type CachingEffects term value effects
  = Fresh
 ': NonDetEff
 ': Reader (CacheFor term value)
 ': State  (CacheFor term value)
 ': effects

-- | The cache for term and abstract value types.
type CacheFor term value = Cache (LocationFor value) term value

newtype CachingAnalysis m term value (effects :: [* -> *]) a = CachingAnalysis { runCachingAnalysis :: m term value effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadEvaluator term value (m term value effects) => MonadEvaluator term value (CachingAnalysis m term value effects)

-- TODO: reabstract these later on

type InCacheEffectFor  term value = Reader (CacheFor term value)
type OutCacheEffectFor term value = State  (CacheFor term value)

askCache :: (Effectful (m term value), Member (InCacheEffectFor term value) effects) => CachingAnalysis m term value effects (CacheFor term value)
askCache = lift ask

localCache :: (Effectful (m term value), Member (InCacheEffectFor term value) effects) => (CacheFor term value -> CacheFor term value) -> CachingAnalysis m term value effects a -> CachingAnalysis m term value effects a
localCache f a = lift (local f (lower a))

asksCache :: (Functor (m term value effects), Effectful (m term value), Member (InCacheEffectFor term value) effects) => (CacheFor term value -> a) -> CachingAnalysis m term value effects a
asksCache f = f <$> askCache

getsCache :: (Functor (m term value effects), Effectful (m term value), Member (OutCacheEffectFor term value) effects) => (CacheFor term value -> a) -> CachingAnalysis m term value effects a
getsCache f = f <$> getCache

getCache :: (Effectful (m term value), Member (OutCacheEffectFor term value) effects) => CachingAnalysis m term value effects (CacheFor term value)
getCache = lift get

putCache :: (Effectful (m term value), Member (OutCacheEffectFor term value) effects) => CacheFor term value -> CachingAnalysis m term value effects ()
putCache = lift . put

modifyCache :: (Effectful (m term value), Member (OutCacheEffectFor term value) effects, Monad (m term value effects)) => (CacheFor term value -> CacheFor term value) -> CachingAnalysis m term value effects ()
modifyCache f = fmap f getCache >>= putCache

-- | This instance coinductively iterates the analysis of a term until the results converge.
instance ( Corecursive term
         , Ord term
         , Ord value
         , Ord (CellFor value)
         , Ord (LocationFor value)
         , Effectful (m term value)
         , MonadFresh (m term value effects)
         , MonadNonDet (m term value effects)
         , Members (CachingEffects term value '[]) effects
         , Evaluatable (Base term)
         , Foldable (Cell (LocationFor value))
         , FreeVariables term
         , MonadAnalysis term value (m term value effects)
         , Recursive term
         )
         => MonadAnalysis term value (CachingAnalysis m term value effects) where
  type RequiredEffects term value (CachingAnalysis m term value effects) = CachingEffects term value (RequiredEffects term value (m term value effects))
  analyzeTerm e = do
    c <- getConfiguration (embedSubterm e)
    -- Convergence here is predicated upon an Eq instance, not α-equivalence
    cache <- converge (\ prevCache -> do
      putCache mempty
      putStore (configurationStore c)
      -- We need to reset fresh generation so that this invocation converges.
      reset 0
      -- This is subtle: though the calling context supports nondeterminism, we want
      -- to corral all the nondeterminism that happens in this @eval@ invocation, so
      -- that it doesn't "leak" to the calling context and diverge (otherwise this
      -- would never complete). We don’t need to use the values, so we 'gather' the
      -- nondeterministic values into @()@.
      _ <- localCache (const prevCache) (gather (memoizeEval e) :: CachingAnalysis m term value effects ())
      getCache) mempty
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
scatter :: (Alternative m, Foldable t, MonadEvaluator term value m) => t (a, Store (LocationFor value) value) -> m a
scatter = getAlt . foldMap (\ (value, store') -> Alt (putStore store' *> pure value))

-- | Evaluation of a single iteration of an analysis, given an in-cache as an oracle for results and an out-cache to record computed results in.
memoizeEval :: ( Ord value
               , Ord term
               , Ord (LocationFor value)
               , Ord (CellFor value)
               , Alternative (m term value effects)
               , Corecursive term
               , FreeVariables term
               , Foldable (Cell (LocationFor value))
               , Functor (Base term)
               , Effectful (m term value)
               , Members (CachingEffects term value '[]) effects
               , Recursive term
               , MonadAnalysis term value (m term value effects)
               -- , Semigroup (CellFor value)
               )
            => SubtermAlgebra (Base term) term (CachingAnalysis m term value effects value)
memoizeEval e = do
  c <- getConfiguration (embedSubterm e)
  cached <- getsCache (cacheLookup c)
  case cached of
    Just pairs -> scatter pairs
    Nothing -> do
      pairs <- asksCache (fromMaybe mempty . cacheLookup c)
      modifyCache (cacheSet c pairs)
      v <- liftAnalyze analyzeTerm e
      store' <- getStore
      modifyCache (cacheInsert c (v, store'))
      pure v
