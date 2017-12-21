{-# LANGUAGE MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.Cache where

import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Cache
import Data.Abstract.Value

-- | 'Monad's offering a readable 'Cache' of values & stores for each configuration in a program.
--
--   This (in-)cache is used as an oracle for the results of recursive computations, allowing us to finitize potentially nonterminating control flow by repeatedly computing the results until analysis converges on a stable value. Each iteration of this process must consult this cache only _after_ evaluating the configuration itself in order to ensure soundness (since it could otherwise produce stale results for some configurations).
--
--   Since finitization crucially depends on convergence, this cache should only be used with value abstractions that will converge for multiple disjoint assignments of a given variable, e.g. its type, and not with precisely-modelled values. To illustrate why, consider a simple incrementing recursive function:
--
--   > inc :: Integer -> a
--   > inc n = inc (n + 1)
--
--   @n@ differs at every iteration, and thus a precise modelling of the integral value will not converge in the store: each iteration will allocate a new address & write a distinct value into it. Modelling values with their types _will_ converge, however, as the type at each iteration is the same.
class Monad m => MonadCacheIn t v m where
  -- | Retrieve the local in-cache.
  askCache :: m (Cache (LocationFor v) t v)

  -- | Run a computation with a locally-modified in-cache.
  localCache :: (Cache (LocationFor v) t v -> Cache (LocationFor v) t v) -> m a -> m a

instance (Reader (Cache (LocationFor v) t v) :< fs) => MonadCacheIn t v (Eff fs) where
  askCache = ask
  localCache = local


-- | 'Monad's offering a readable & writable 'Cache' of values & stores for each configuration in a program.
--
--   This (out-)cache is used to store the results of recursive computations, allowing us to finitize each iteration of an analysis by first looking up the current configuration in the cache and only evaluating:
--
--   1. If the configuration has not been visited before, and
--   2. _after_ copying the previous iteration’s results (from the in-cache, and defaulting to a 'mempty' set of results) into the out-cache.
--
--   Thus, visiting the same configuration twice recursively will terminate, since we’ll have consulted the in-cache as an oracle before evaluating, and after evaluating, the resulting value and store should be appended into the out-cache. Then, once the current iteration of the analysis has completed, the updated out-cache will be used as the oracle for the next iteration, until such time as the cache converges.
--
--   See also 'MonadCacheIn' for discussion of the conditions of finitization.
class Monad m => MonadCacheOut t v m where
  -- | Retrieve the current out-cache.
  getCache :: m (Cache (LocationFor v) t v)

  -- | Update the current out-cache.
  putCache :: Cache (LocationFor v) t v -> m ()

instance (State (Cache (LocationFor v) t v) :< fs) => MonadCacheOut t v (Eff fs) where
  getCache = get
  putCache = put

-- | Modify the current out-cache using a given function.
modifyCache :: MonadCacheOut t v m => (Cache (LocationFor v) t v -> Cache (LocationFor v) t v) -> m ()
modifyCache f = fmap f getCache >>= putCache
