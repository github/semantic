{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, UndecidableInstances #-}
module Analysis.Abstract.Caching
  ( evaluateCache )
  where

import Prologue
import Data.Monoid (Alt(..))
import Control.Abstract.Evaluator
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.NonDet
import Data.Abstract.Address
import Data.Abstract.Cache
import Data.Abstract.Configuration
import Data.Abstract.Environment
import Data.Abstract.Evaluatable
import Data.Abstract.Linker
import Data.Abstract.Live
import Data.Abstract.Store
import Data.Abstract.Value
import qualified Data.Set as Set

-- | The effects necessary for caching analyses.
type CachingEffects t v
  = '[ Fresh                                  -- For 'MonadFresh'.
     , Reader (Live (LocationFor v) v)        -- For 'MonadGC'.
     , Reader (Environment (LocationFor v) v) -- For 'MonadEnv'.
     , State (Environment (LocationFor v) v)  -- For 'MonadEvaluator'.
     , Fail                                   -- For 'MonadFail'.
     , NonDetEff                              -- For 'Alternative' & 'MonadNonDet'.
     , State (Store (LocationFor v) v)        -- For 'MonadStore'.
     , Reader (Cache (LocationFor v) t v)     -- For 'MonadCacheIn'.
     , State (Cache (LocationFor v) t v)      -- For 'MonadCacheOut'.
     , Reader (Linker t)                      -- Cache of unevaluated modules
     , State (Linker v)                       -- Cache of evaluated modules
     ]

newtype CachingAnalysis term value a = CachingAnalysis { runCachingAnalysis :: Evaluator (CachingEffects term value) term value a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadEvaluator term value (CachingAnalysis term value)

-- TODO: reabstract these later on

askCache :: CachingAnalysis t v (Cache (LocationFor v) t v)
askCache = CachingAnalysis (Evaluator ask)

localCache :: (Cache (LocationFor v) t v -> Cache (LocationFor v) t v) -> CachingAnalysis t v a -> CachingAnalysis t v a
localCache f (CachingAnalysis (Evaluator a)) = CachingAnalysis (Evaluator (local f a))

asksCache :: (Cache (LocationFor v) t v -> a) -> CachingAnalysis t v a
asksCache f = f <$> askCache

getsCache :: (Cache (LocationFor v) t v -> a) -> CachingAnalysis t v a
getsCache f = f <$> getCache

getCache :: CachingAnalysis t v (Cache (LocationFor v) t v)
getCache = CachingAnalysis (Evaluator get)

putCache :: Cache (LocationFor v) t v -> CachingAnalysis t v ()
putCache v = CachingAnalysis (Evaluator (put v))

modifyCache :: (Cache (LocationFor v) t v -> Cache (LocationFor v) t v) -> CachingAnalysis t v ()
modifyCache f = fmap f getCache >>= putCache

-- | This instance coinductively iterates the analysis of a term until the results converge.
instance ( Corecursive t
         , Ord t
         , Ord v
         , Ord (Cell (LocationFor v) v)
         , Evaluatable (Base t)
         , Foldable (Cell (LocationFor v))
         , FreeVariables t
         , MonadAddressable (LocationFor v) v (CachingAnalysis t v)
         , MonadValue t v (CachingAnalysis t v)
         , Recursive t
         , Semigroup (Cell (LocationFor v) v)
         )
         => MonadAnalysis t v (CachingAnalysis t v) where
  analyzeTerm e = do
    c <- getConfiguration (embedSubterm e)
    -- Convergence here is predicated upon an Eq instance, not α-equivalence
    cache <- converge (\ prevCache -> do
      putCache (mempty :: Cache (LocationFor v) t v)
      putStore (configurationStore c)
      -- We need to reset fresh generation so that this invocation converges.
      reset 0
      -- This is subtle: though the calling context supports nondeterminism, we want
      -- to corral all the nondeterminism that happens in this @eval@ invocation, so
      -- that it doesn't "leak" to the calling context and diverge
      -- (otherwise this would never complete).
      _ <- localCache (const prevCache) (gather Set.singleton (memoizeEval e))
      getCache) mempty
    maybe empty scatter (cacheLookup c cache)


-- | Coinductively-cached evaluation.
evaluateCache :: forall v term
              . ( Ord v
                , Ord term
                , Ord (LocationFor v)
                , Ord (Cell (LocationFor v) v)
                , Corecursive term
                , Evaluatable (Base term)
                , FreeVariables term
                , Foldable (Cell (LocationFor v))
                , Functor (Base term)
                , Recursive term
                , MonadAddressable (LocationFor v) v (CachingAnalysis term v)
                , MonadValue term v (CachingAnalysis term v)
                , Semigroup (Cell (LocationFor v) v)
                , ValueRoots (LocationFor v) v
                )
              => term
              -> Final (CachingEffects term v) v
evaluateCache = run @(CachingEffects term v) . runEvaluator . runCachingAnalysis . evaluateTerm

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
scatter :: (Alternative m, Foldable t, MonadEvaluator term v m) => t (a, Store (LocationFor v) v) -> m a
scatter = getAlt . foldMap (\ (value, store') -> Alt (putStore store' *> pure value))

-- | Evaluation of a single iteration of an analysis, given a 'MonadCacheIn' instance as an oracle for results and a 'MonadCacheOut' instance to record computed results in.
memoizeEval :: forall v term
            . ( Ord v
              , Ord term
              , Ord (LocationFor v)
              , Ord (Cell (LocationFor v) v)
              , Corecursive term
              , Evaluatable (Base term)
              , FreeVariables term
              , Foldable (Cell (LocationFor v))
              , Functor (Base term)
              , Recursive term
              , MonadAddressable (LocationFor v) v (CachingAnalysis term v)
              , MonadValue term v (CachingAnalysis term v)
              , Semigroup (Cell (LocationFor v) v)
              )
            => SubtermAlgebra (Base term) term (CachingAnalysis term v v)
memoizeEval e = do
  c <- getConfiguration (embedSubterm e)
  cached <- getsCache (cacheLookup c)
  case cached of
    Just pairs -> scatter pairs
    Nothing -> do
      pairs <- asksCache (fromMaybe mempty . cacheLookup c)
      modifyCache (cacheSet c pairs)
      v <- eval e
      store' <- getStore
      modifyCache (cacheInsert c (v, store'))
      pure v

{-


-- | Evaluation of a single iteration of an analysis, given a 'MonadCacheIn' instance as an oracle for results and a 'MonadCacheOut' instance to record computed results in.
evCache :: forall t v m
        . ( Ord (LocationFor v)
          , Ord t
          , Ord v
          , Ord (Cell (LocationFor v) v)
          , MonadCaching t v m
          )
        => (((v -> m v) -> t -> m v) -> (v -> m v) -> t -> m v)
        -> ((v -> m v) -> t -> m v)
        -> (v -> m v) -> t -> m v


-}
