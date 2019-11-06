{-# LANGUAGE DeriveFunctor, FlexibleContexts, GeneralizedNewtypeDeriving, TypeApplications, TypeOperators #-}
module Analysis.Abstract.Caching.FlowInsensitive
( cachingTerms
, convergingModules
, caching
) where

import Control.Abstract
import Data.Abstract.Module
import Data.Map.Monoidal as Monoidal hiding (empty)
import Prologue

-- | Look up the set of values for a given configuration in the in-cache.
consultOracle :: (Member (Reader (Cache term address value)) sig, Carrier sig m, Ord address, Ord term, Ord value)
              => Configuration term address
              -> Evaluator term address value m (Set value)
consultOracle configuration = asks (fromMaybe mempty . cacheLookup configuration)

-- | Run an action with the given in-cache.
withOracle :: (Member (Reader (Cache term address value)) sig, Carrier sig m)
           => Cache term address value
           -> Evaluator term address value m a
           -> Evaluator term address value m a
withOracle cache = local (const cache)


-- | Look up the set of values for a given configuration in the out-cache.
lookupCache :: (Member (State (Cache term address value)) sig, Carrier sig m, Ord address, Ord term)
            => Configuration term address
            -> Evaluator term address value m (Maybe (Set value))
lookupCache configuration = cacheLookup configuration <$> get

-- | Run an action, caching its result and 'Heap' under the given configuration.
cachingConfiguration :: (Member (State (Cache term address value)) sig, Carrier sig m, Ord address, Ord term, Ord value)
                     => Configuration term address
                     -> Set value
                     -> Evaluator term address value m value
                     -> Evaluator term address value m value
cachingConfiguration configuration values action = do
  modify (cacheSet configuration values)
  result <- action
  result <$ modify (cacheInsert configuration result)

putCache :: (Member (State (Cache term address value)) sig, Carrier sig m)
         => Cache term address value
         -> Evaluator term address value m ()
putCache = put

-- | Run an action starting from an empty out-cache, and return the out-cache afterwards.
isolateCache :: (Member (State (Cache term address value)) sig, Member (State (Heap address address value)) sig, Carrier sig m)
             => Evaluator term address value m a
             -> Evaluator term address value m (Cache term address value, Heap address address value)
isolateCache action = putCache lowerBound *> action *> ((,) <$> get <*> get)


-- | Analyze a term using the in-cache as an oracle & storing the results of the analysis in the out-cache.
cachingTerms :: ( Member (Reader (Cache term address value)) sig
                , Member (Reader (Live address)) sig
                , Member (State (Cache term address value)) sig
                , Carrier sig m
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

convergingModules :: ( Eq value
                     , Member Fresh sig
                     , Member (Reader (Cache term address value)) sig
                     , Member (Reader (Live address)) sig
                     , Member (State (Cache term address value)) sig
                     , Member (State (Heap address address value)) sig
                     , Ord address
                     , Ord term
                     , Carrier sig m
                     , Alternative m
                     )
                  => (Module (Either prelude term) -> Evaluator term address value (NonDetC m) value)
                  -> (Module (Either prelude term) -> Evaluator term address value m value)
convergingModules recur m@(Module _ (Left _)) = raiseHandler runNonDet (recur m) >>= maybeM empty
convergingModules recur m@(Module _ (Right term)) = do
  c <- getConfiguration term
  heap <- getHeap
  -- Convergence here is predicated upon an Eq instance, not α-equivalence
  (cache, _) <- converge (lowerBound, heap) (\ (prevCache, _) -> isolateCache $ do
    -- We need to reset fresh generation so that this invocation converges.
    resetFresh $
    -- This is subtle: though the calling context supports nondeterminism, we want
    -- to corral all the nondeterminism that happens in this @eval@ invocation, so
    -- that it doesn't "leak" to the calling context and diverge (otherwise this
    -- would never complete). We don’t need to use the values, so we 'gather' the
    -- nondeterministic values into @()@.
      withOracle prevCache (raiseHandler (runNonDet @Maybe) (recur m)))
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
scatter :: (Foldable t, Carrier sig m, Alternative m) => t value -> Evaluator term address value m value
scatter = foldMapA pure

-- | Get the current 'Configuration' with a passed-in term.
getConfiguration :: (Member (Reader (Live address)) sig, Carrier sig m)
                 => term
                 -> Evaluator term address value m (Configuration term address)
getConfiguration term = Configuration term <$> askRoots


caching :: Carrier sig m
        => Evaluator term address value (NonDetC
                                        (ReaderC (Cache term address value)
                                        (StateC (Cache term address value)
                                        m))) a
        -> Evaluator term address value m (Cache term address value, [a])
caching
  = raiseHandler (runState  lowerBound)
  . raiseHandler (runReader lowerBound)
  . fmap (toList @B)
  . raiseHandler runNonDet

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
  { configurationTerm    :: term                -- ^ The “instruction,” i.e. the current term to evaluate.
  , configurationRoots   :: Live address        -- ^ The set of rooted addresses.
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
