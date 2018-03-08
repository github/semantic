{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
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
type CachingEffectsFor m
  = '[ Fresh               -- For 'MonadFresh'. TODO: Extract typing constraints into a separate analysis.
     , NonDetEff           -- For 'Alternative' & 'MonadNonDet'.
     , Reader (CacheFor m) -- For the in-cache.
     , State  (CacheFor m) -- For the out-cache
     ]

type CachingEffects term value effects
  = Fresh
 ': NonDetEff
 ': Reader (Cache (LocationFor value) term value)
 ': State  (Cache (LocationFor value) term value)
 ': effects

-- | The cache for term and abstract value types.
type CacheFor m = Cache (LocationFor (ValueFor m)) (TermFor m) (ValueFor m)

newtype CachingAnalysis m a = CachingAnalysis { runCachingAnalysis :: m a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadEvaluator, MonadFail, MonadFresh, MonadNonDet)

-- TODO: reabstract these later on

type InCacheEffectFor  m = Reader (CacheFor m)
type OutCacheEffectFor m = State  (CacheFor m)

askCache :: (Effectful m, Member (InCacheEffectFor m) (EffectsFor m)) => CachingAnalysis m (CacheFor m)
askCache = lift ask

localCache :: (Effectful m, Member (InCacheEffectFor m) (EffectsFor m)) => (CacheFor m -> CacheFor m) -> CachingAnalysis m a -> CachingAnalysis m a
localCache f a = lift (local f (lower a))

asksCache :: (Functor m, Effectful m, Member (InCacheEffectFor m) (EffectsFor m)) => (CacheFor m -> a) -> CachingAnalysis m a
asksCache f = f <$> askCache

getsCache :: (Functor m, Effectful m, Member (OutCacheEffectFor m) (EffectsFor m)) => (CacheFor m -> a) -> CachingAnalysis m a
getsCache f = f <$> getCache

getCache :: (Effectful m, Member (OutCacheEffectFor m) (EffectsFor m)) => CachingAnalysis m (CacheFor m)
getCache = lift get

putCache :: (Effectful m, Member (OutCacheEffectFor m) (EffectsFor m)) => CacheFor m -> CachingAnalysis m ()
putCache = lift . put

modifyCache :: (Effectful m, Member (OutCacheEffectFor m) (EffectsFor m), Monad m) => (CacheFor m -> CacheFor m) -> CachingAnalysis m ()
modifyCache f = fmap f getCache >>= putCache

-- | This instance coinductively iterates the analysis of a term until the results converge.
instance ( Corecursive (TermFor m)
         , Ord (TermFor m)
         , Ord (ValueFor m)
         , Ord (CellFor (ValueFor m))
         , Ord (LocationFor (ValueFor m))
         , Effectful m
         , MonadFresh m
         , MonadNonDet m
         , Members (CachingEffectsFor m) (EffectsFor m)
         , Evaluatable (Base (TermFor m))
         , Foldable (Cell (LocationFor (ValueFor m)))
         , FreeVariables (TermFor m)
         , MonadAnalysis m
         , Recursive (TermFor m)
         )
         => MonadAnalysis (CachingAnalysis m) where
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
      _ <- localCache (const prevCache) (gather (memoizeEval e) :: CachingAnalysis m ())
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
scatter :: (Alternative m, Foldable t, MonadEvaluator m) => t (a, Store (LocationFor (ValueFor m)) (ValueFor m)) -> m a
scatter = getAlt . foldMap (\ (value, store') -> Alt (putStore store' *> pure value))

-- | Evaluation of a single iteration of an analysis, given an in-cache as an oracle for results and an out-cache to record computed results in.
memoizeEval :: ( Ord (ValueFor m)
               , Ord (TermFor m)
               , Ord (LocationFor (ValueFor m))
               , Ord (CellFor (ValueFor m))
               , Alternative m
               , Corecursive (TermFor m)
               , FreeVariables (TermFor m)
               , Foldable (Cell (LocationFor (ValueFor m)))
               , Functor (Base (TermFor m))
               , Effectful m
               , Members (CachingEffectsFor m) (EffectsFor m)
               , Recursive (TermFor m)
               , MonadAnalysis m
               -- , Semigroup (CellFor (ValueFor m))
               )
            => SubtermAlgebra (Base (TermFor m)) (TermFor m) (CachingAnalysis m (ValueFor m))
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
