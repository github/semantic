{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables, TypeOperators #-}
module Analysis.FlowInsensitive
( Heap
, FrameId(..)
, convergeTerm
, cacheTerm
, runHeap
, foldMapA
) where

import           Control.Effect
import           Control.Effect.Fresh
import           Control.Effect.NonDet
import           Control.Effect.Reader
import           Control.Effect.State
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Alt(..))
import qualified Data.Set as Set

newtype Cache term a = Cache { unCache :: Map.Map term (Set.Set a) }
  deriving (Eq, Ord, Show)

type Heap address a = Map.Map address (Set.Set a)

newtype FrameId name = FrameId { unFrameId :: name }
  deriving (Eq, Ord, Show)


convergeTerm :: forall m sig a term address proxy
             .  ( Carrier sig m
                , Effect sig
                , Eq address
                , Member Fresh sig
                , Member (State (Heap address a)) sig
                , Ord a
                , Ord term
                )
             => proxy address
             -> (term -> NonDetC (ReaderC (Cache term a) (StateC (Cache term a) m)) a)
             -> term
             -> m (Set.Set a)
convergeTerm _ eval body = do
  heap <- get
  (cache, _) <- converge (Cache Map.empty :: Cache term a, heap :: Heap address a) $ \ (prevCache, _) -> runState (Cache Map.empty) . runReader prevCache $ do
    _ <- resetFresh . runNonDetM Set.singleton $ eval body
    get
  pure (fromMaybe mempty (Map.lookup body (unCache cache)))

cacheTerm :: forall m sig a term
          .  ( Alternative m
             , Carrier sig m
             , Member (Reader (Cache term a)) sig
             , Member (State  (Cache term a)) sig
             , Ord a
             , Ord term
             )
          => (term -> m a)
          -> (term -> m a)
cacheTerm eval term = do
  cached <- gets (Map.lookup term . unCache)
  case cached :: Maybe (Set.Set a) of
    Just results -> foldMapA pure results
    Nothing -> do
      results <- asks (fromMaybe mempty . Map.lookup term . unCache)
      modify (Cache . Map.insert term (results :: Set.Set a) . unCache)
      result <- eval term
      result <$ modify (Cache . Map.insertWith (<>) term (Set.singleton (result :: a)) . unCache)

runHeap :: StateC (Heap address a) m b -> m (Heap address a, b)
runHeap m = runState Map.empty m

-- | Fold a collection by mapping each element onto an 'Alternative' action.
foldMapA :: (Alternative m, Foldable t) => (b -> m a) -> t b -> m a
foldMapA f = getAlt . foldMap (Alt . f)

runNonDetM :: (Monoid b, Applicative m) => (a -> b) -> NonDetC m a -> m b
runNonDetM f (NonDetC m) = m (fmap . (<>) . f) (pure mempty)

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
