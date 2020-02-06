{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Analysis.Abstract.Caching.FlowInsensitive
( cachingTerms
, convergingModules
, caching
) where

import Control.Carrier.Fresh.Strict
import Control.Carrier.NonDet.Church
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Data.Bifunctor
import Data.Foldable
import Data.Functor.Classes
import Data.Maybe.Exts
import Data.Semilattice.Lower
import Data.Set (Set)

import Control.Abstract
import Data.Abstract.Module
import Data.Map.Monoidal as Monoidal hiding (empty)

-- | Look up the set of values for a given configuration in the in-cache.
consultOracle :: (Has (Reader (Cache term address value)) sig m, Ord address, Ord term, Ord value)
              => Configuration term address
              -> Evaluator term address value m (Set value)
consultOracle configuration = asks (fromMaybe mempty . cacheLookup configuration)

-- | Run an action with the given in-cache.
withOracle :: Has (Reader (Cache term address value)) sig m
           => Cache term address value
           -> Evaluator term address value m a
           -> Evaluator term address value m a
withOracle cache = local (const cache)


-- | Look up the set of values for a given configuration in the out-cache.
lookupCache :: (Has (State (Cache term address value)) sig m, Ord address, Ord term)
            => Configuration term address
            -> Evaluator term address value m (Maybe (Set value))
lookupCache configuration = cacheLookup configuration <$> get

-- | Run an action, caching its result and 'Heap' under the given configuration.
cachingConfiguration :: (Has (State (Cache term address value)) sig m, Ord address, Ord term, Ord value)
                     => Configuration term address
                     -> Set value
                     -> Evaluator term address value m value
                     -> Evaluator term address value m value
cachingConfiguration configuration values action = do
  modify (cacheSet configuration values)
  result <- action
  result <$ modify (cacheInsert configuration result)

putCache :: Has (State (Cache term address value)) sig m
         => Cache term address value
         -> Evaluator term address value m ()
putCache = put

-- | Run an action starting from an empty out-cache, and return the out-cache afterwards.
isolateCache :: (Has (State (Cache term address value)) sig m, Has (State (Heap address address value)) sig m)
             => Evaluator term address value m a
             -> Evaluator term address value m (Cache term address value, Heap address address value)
isolateCache action = putCache lowerBound *> action *> ((,) <$> get <*> get)


-- | Analyze a term using the in-cache as an oracle & storing the results of the analysis in the out-cache.
cachingTerms :: ( Has (Reader (Cache term address value)) sig m
                , Has (Reader (Live address)) sig m
                , Has (State (Cache term address value)) sig m
                , Ord address
                , Ord term
                , Ord value
                , Alternative m
                )
             => Open (term -> Evaluator term address value m value)
cachingTerms recur term = do
  c <- getConfiguration term
  cached <- lookupCache c
  case cached of
    Just values -> scatter values
    Nothing -> do
      values <- consultOracle c
      cachingConfiguration c values (recur term)

convergingModules :: ( Effect sig
                     , Eq value
                     , Has Fresh sig m
                     , Has (Reader (Cache term address value)) sig m
                     , Has (Reader (Live address)) sig m
                     , Has (State (Cache term address value)) sig m
                     , Has (State (Heap address address value)) sig m
                     , Ord address
                     , Ord term
                     , Alternative m
                     )
                  => (Module (Either prelude term) -> Evaluator term address value (NonDetC (FreshC m)) value)
                  -> (Module (Either prelude term) -> Evaluator term address value m value)
convergingModules recur m@(Module _ (Left _)) = raiseHandler (evalFresh 0 . runNonDetA) (recur m) >>= maybeM empty
convergingModules recur m@(Module _ (Right term)) = do
  c <- getConfiguration term
  heap <- getHeap
  -- Convergence here is predicated upon an Eq instance, not α-equivalence
  (cache, _) <- converge (lowerBound, heap) (\ (prevCache, _) -> isolateCache $ do
    -- We need to reset fresh generation so that this invocation converges.
    raiseHandler (evalFresh 0) $
    -- This is subtle: though the calling context supports nondeterminism, we want
    -- to corral all the nondeterminism that happens in this @eval@ invocation, so
    -- that it doesn't "leak" to the calling context and diverge (otherwise this
    -- would never complete). We don’t need to use the values, so we 'gather' the
    -- nondeterministic values into @()@.
      withOracle prevCache (raiseHandler (runNonDetA @Maybe) (recur m)))
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
scatter :: (Foldable t, Alternative m) => t value -> Evaluator term address value m value
scatter = foldMapA pure

-- | Get the current 'Configuration' with a passed-in term.
getConfiguration :: Has (Reader (Live address)) sig m
                 => term
                 -> Evaluator term address value m (Configuration term address)
getConfiguration term = Configuration term <$> askRoots


caching :: Algebra sig m
        => Evaluator term address value (NonDetC
                                        (ReaderC (Cache term address value)
                                        (StateC (Cache term address value)
                                        m))) a
        -> Evaluator term address value m (Cache term address value, [a])
caching
  = raiseHandler (runState  lowerBound)
  . raiseHandler (runReader lowerBound)
  . fmap (toList @B)
  . raiseHandler runNonDetA

data B a = E | L a | B (B a) (B a)
  deriving (Functor)

instance Foldable B where
  toList = flip go []
    where go E       rest = rest
          go (L a)   rest = a : rest
          go (B a b) rest = go a (go b rest)

  foldMap f = go
    where go E       = mempty
          go (L a)   = f a
          go (B a b) = go a <> go b

  null E = True
  null _ = False

instance Traversable B where
  traverse f = go
    where go E       = pure E
          go (L a)   = L <$> f a
          go (B a b) = B <$> go a <*> go b

instance Applicative B where
  pure = L
  E     <*> _ = E
  L f   <*> a = fmap f a
  B l r <*> a = B (l <*> a) (r <*> a)

instance Alternative B where
  empty = E
  E <|> b = b
  a <|> E = a
  a <|> b = B a b

instance Monad B where
  return = pure
  E     >>= _ = E
  L a   >>= f = f a
  B l r >>= f = B (l >>= f) (r >>= f)


-- | A map of 'Configuration's to 'Set's of resulting values & 'Heap's.
newtype Cache term address value = Cache { unCache :: Monoidal.Map (Configuration term address) (Set value) }
  deriving (Eq, Lower, Monoid, Ord, Reducer (Configuration term address, value), Semigroup)

-- | A single point in a program’s execution.
data Configuration term address = Configuration
  { configurationTerm  :: term                -- ^ The “instruction,” i.e. the current term to evaluate.
  , configurationRoots :: Live address        -- ^ The set of rooted addresses.
  }
  deriving (Eq, Ord, Show)


-- | Look up the resulting value & 'Heap' for a given 'Configuration'.
cacheLookup :: (Ord address, Ord term) => Configuration term address -> Cache term address value -> Maybe (Set value)
cacheLookup key = Monoidal.lookup key . unCache

-- | Set the resulting value & 'Heap' for a given 'Configuration', overwriting any previous entry.
cacheSet :: (Ord address, Ord term) => Configuration term address -> Set value -> Cache term address value -> Cache term address value
cacheSet key value = Cache . Monoidal.insert key value . unCache

-- | Insert the resulting value & 'Heap' for a given 'Configuration', appending onto any previous entry.
cacheInsert :: (Ord address, Ord term, Ord value) => Configuration term address -> value -> Cache term address value -> Cache term address value
cacheInsert = curry cons

instance (Show term, Show address, Show value) => Show (Cache term address value) where
  showsPrec d = showsUnaryWith showsPrec "Cache" d . map (second toList) . Monoidal.pairs . unCache
