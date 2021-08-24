{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Analysis.FlowInsensitive
( -- * Flow-insensitive convergence
  convergeTerm
, cacheTerm
, CacheC(..)
, runCache
  -- * Kleene least fixed-point theorem
, converge
, convergeBy
, convergeMaybe
, convergeEither
  -- * Caches
, Cache(..)
, emptyCache
, lookupCache
, insertCache
, insertsCache
) where

import           Analysis.Carrier.Store.Monovariant
import           Control.Algebra
import           Control.Carrier.NonDet.Church
import           Control.Carrier.Reader
import           Control.Carrier.State.Church
import           Control.Monad ((<=<))
import           Control.Monad.Fail as Fail
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set

convergeTerm
  :: forall term value m sig
  .  ( Has (State (MStore value)) sig m
     , Ord term
     , Ord value
     )
  => (term -> CacheC term value m value)
  -> term
  -> m (Set.Set value)
convergeTerm eval term = do
  heap <- get @(MStore value)
  lookupCache term . fst <$> converge (\ (prevCache, _) -> runCache prevCache (eval term)) (emptyCache, heap)

cacheTerm
  :: ( Alternative m
     , Has (Reader (Cache term value)) sig m
     , Has (State  (Cache term value)) sig m
     , Ord term
     , Ord value
     )
  => (term -> m value)
  -> (term -> m value)
cacheTerm eval term = do
  cached <- gets (lookupCache term)
  if Set.null cached then do
    results <- asks (lookupCache term)
    modify (insertsCache term (results `asTypeOf` cached))
    result <- eval term
    result <$ modify (insertCache term result)
  else
    foldMapA pure cached


newtype CacheC term value m a = CacheC (NonDetC (ReaderC (Cache term value) (StateC (Cache term value) m)) a)
  deriving (Algebra (NonDet :+: Reader (Cache term value) :+: State (Cache term value) :+: sig), Alternative, Applicative, Functor, Monad, Fail.MonadFail)

runCache :: (Has (State (MStore value)) sig m, Ord a) => Cache term value -> CacheC term value m a -> m (Cache term value, MStore value)
runCache prevCache (CacheC m) = runState (curry pure) emptyCache (runReader prevCache (runNonDetM Set.singleton m *> get))


-- Kleene least fixed-point theorem

-- | Iterate a monadic action starting from some initial seed until the results converge.
--
-- This applies the Kleene fixed-point theorem to finitize a monotone action. cf https://en.wikipedia.org/wiki/Kleene_fixed-point_theorem
converge
  :: (Eq a, Monad m)
  => (a -> m a) -- ^ A monadic action to perform at each iteration, starting from the result of the previous iteration or from the seed value for the first iteration.
  -> a          -- ^ An initial seed value to iterate from.
  -> m a        -- ^ A computation producing the least fixed point (the first value at which the actions converge).
converge = convergeBy (==)

-- | Iterate a monadic action starting from some initial seed until the results converge according to the passed test.
--
-- This applies the Kleene fixed-point theorem to finitize a monotone action. cf https://en.wikipedia.org/wiki/Kleene_fixed-point_theorem
convergeBy
  :: Monad m
  => (a -> a -> Bool) -- ^ A function to use as the equality test to determine convergence.
  -> (a -> m a)       -- ^ A monadic action to perform at each iteration, starting from the result of the previous iteration or from the seed value for the first iteration.
  -> a                -- ^ An initial seed value to iterate from.
  -> m a              -- ^ A computation producing the least fixed point (the first value at which the actions converge).
convergeBy (==) f = convergeMaybe (\ x -> qualify x <$> f x)
  where
  qualify x x'
    | x == x'   = Nothing
    | otherwise = Just x'

-- | Iterate a monadic action starting from some initial seed until the results converge.
--
-- This applies the Kleene fixed-point theorem to finitize a monotone action. cf https://en.wikipedia.org/wiki/Kleene_fixed-point_theorem
convergeMaybe
  :: Monad m
  => (a -> m (Maybe a)) -- ^ A monadic action to perform at each iteration, starting from the result of the previous iteration or from the seed value for the first iteration. Returns of 'Nothing' end iteration, while 'Just' begins another iteration.
  -> a                  -- ^ An initial seed value to iterate from.
  -> m a                -- ^ A computation producing the least fixed point (the first value at which the actions converge).
convergeMaybe f = convergeEither (\ x -> maybe (Left x) Right <$> f x)

-- | Iterate a monadic action starting from some initial seed until the results converge.
--
-- This applies the Kleene fixed-point theorem to finitize a monotone action. cf https://en.wikipedia.org/wiki/Kleene_fixed-point_theorem
convergeEither
  :: Monad m
  => (a -> m (Either b a)) -- ^ A monadic action to perform at each iteration, starting from the result of the previous iteration or from the seed value for the first iteration. Returns of 'Left' end iteration, while 'Right' begins another iteration.
  -> a                     -- ^ An initial seed value to iterate from.
  -> m b                   -- ^ A computation producing the least fixed point (the first value at which the actions converge).
convergeEither f = loop
  where
  loop = either pure loop <=< f


-- Caches

newtype Cache term value = Cache { getCache :: Map.Map term (Set.Set value) }
  deriving (Eq, Ord, Show)

instance (Ord term, Ord value) => Semigroup (Cache term value) where
  Cache a <> Cache b = Cache (Map.unionWith (<>) a b)

instance (Ord term, Ord value) => Monoid (Cache term value) where
  mempty = emptyCache

emptyCache :: Cache term value
emptyCache = Cache Map.empty

lookupCache :: Ord term => term -> Cache term value -> Set.Set value
lookupCache k (Cache m) = fromMaybe Set.empty (Map.lookup k m)

insertCache :: (Ord term, Ord value) => term -> value -> Cache term value -> Cache term value
insertCache term value (Cache m) = Cache (Map.insertWith (<>) term (Set.singleton value) m)

insertsCache :: (Ord term, Ord value) => term -> Set.Set value -> Cache term value -> Cache term value
insertsCache term values (Cache m) = Cache (Map.insertWith (<>) term values m)
