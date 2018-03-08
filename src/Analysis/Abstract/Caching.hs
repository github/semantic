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

newtype CachingAnalysis m (effects :: [* -> *]) a = CachingAnalysis { runCachingAnalysis :: m effects a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance Effectful effects (m effects) => Effectful effects (CachingAnalysis m effects)

deriving instance MonadEvaluator effects m => MonadEvaluator effects (CachingAnalysis m)
-- TODO: reabstract these later on

type InCacheEffectFor  m = Reader (CacheFor m)
type OutCacheEffectFor m = State  (CacheFor m)

askCache :: (Effectful effects (m effects), Member (InCacheEffectFor m) effects) => CachingAnalysis m effects (CacheFor m)
askCache = lift ask

localCache :: (Effectful effects (m effects), Member (InCacheEffectFor m) effects) => (CacheFor m -> CacheFor m) -> CachingAnalysis m effects a -> CachingAnalysis m effects a
localCache f a = lift (local f (lower a))

asksCache :: (Functor (m effects), Effectful effects (m effects), Member (InCacheEffectFor m) effects) => (CacheFor m -> a) -> CachingAnalysis m effects a
asksCache f = f <$> askCache

getsCache :: (Functor (m effects), Effectful effects (m effects), Member (OutCacheEffectFor m) effects) => (CacheFor m -> a) -> CachingAnalysis m effects a
getsCache f = f <$> getCache

getCache :: (Effectful effects (m effects), Member (OutCacheEffectFor m) effects) => CachingAnalysis m effects (CacheFor m)
getCache = lift get

putCache :: (Effectful effects (m effects), Member (OutCacheEffectFor m) effects) => CacheFor m -> CachingAnalysis m effects ()
putCache = lift . put

modifyCache :: (Effectful effects (m effects), Member (OutCacheEffectFor m) effects, Monad (m effects)) => (CacheFor m -> CacheFor m) -> CachingAnalysis m effects ()
modifyCache f = fmap f getCache >>= putCache

-- | This instance coinductively iterates the analysis of a term until the results converge.
instance ( Corecursive (TermFor m)
         , Ord (TermFor m)
         , Ord (ValueFor m)
         , Ord (CellFor (ValueFor m))
         , Ord (LocationFor (ValueFor m))
         , Effectful effects (m effects)
         , MonadFresh (m effects)
         , MonadNonDet (m effects)
         , Members (CachingEffectsFor m) effects
         , Evaluatable (Base (TermFor m))
         , Foldable (Cell (LocationFor (ValueFor m)))
         , FreeVariables (TermFor m)
         , MonadAnalysis effects m
         , Recursive (TermFor m)
         )
         => MonadAnalysis effects (CachingAnalysis m) where
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
      _ <- localCache (const prevCache) (gather (memoizeEval e) :: CachingAnalysis m effects ())
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
scatter :: (Alternative (m effects), Foldable t, MonadEvaluator effects m) => t (a, Store (LocationFor (ValueFor m)) (ValueFor m)) -> m effects a
scatter = getAlt . foldMap (\ (value, store') -> Alt (putStore store' *> pure value))

-- | Evaluation of a single iteration of an analysis, given an in-cache as an oracle for results and an out-cache to record computed results in.
memoizeEval :: ( Ord (ValueFor m)
               , Ord (TermFor m)
               , Ord (LocationFor (ValueFor m))
               , Ord (CellFor (ValueFor m))
               , Alternative (m effects)
               , Corecursive (TermFor m)
               , FreeVariables (TermFor m)
               , Foldable (Cell (LocationFor (ValueFor m)))
               , Functor (Base (TermFor m))
               , Effectful effects (m effects)
               , Members (CachingEffectsFor m) effects
               , Recursive (TermFor m)
               , MonadAnalysis effects m
               )
            => SubtermAlgebra (Base (TermFor m)) (TermFor m) (CachingAnalysis m effects (ValueFor m))
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
