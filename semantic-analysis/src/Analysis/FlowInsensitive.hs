{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables, TypeOperators #-}
module Analysis.FlowInsensitive
( Heap
, convergeTerm
, cacheTerm
, runHeap
, foldMapA
) where

import           Analysis.Name
import           Control.Effect
import           Control.Effect.Fresh
import           Control.Effect.NonDet
import           Control.Effect.Reader
import           Control.Effect.State
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Alt(..))
import qualified Data.Set as Set

newtype Cache term value = Cache { unCache :: Map.Map term (Set.Set value) }
  deriving (Eq, Ord, Show)

type Heap value = Map.Map Name (Set.Set value)


convergeTerm :: forall term value m sig
             .  ( Carrier sig m
                , Effect sig
                , Member Fresh sig
                , Member (State (Heap value)) sig
                , Ord term
                , Ord value
                )
             => (term -> NonDetC (ReaderC (Cache term value) (StateC (Cache term value) m)) value)
             -> term
             -> m (Set.Set value)
convergeTerm eval body = do
  heap <- get
  (cache, _) <- converge (Cache Map.empty :: Cache term value, heap :: Heap value) $ \ (prevCache, _) -> runState (Cache Map.empty) . runReader prevCache $ do
    _ <- resetFresh . runNonDetM Set.singleton $ eval body
    get
  pure (fromMaybe mempty (Map.lookup body (unCache cache)))

cacheTerm :: forall term value m sig
          .  ( Alternative m
             , Carrier sig m
             , Member (Reader (Cache term value)) sig
             , Member (State  (Cache term value)) sig
             , Ord term
             , Ord value
             )
          => (term -> m value)
          -> (term -> m value)
cacheTerm eval term = do
  cached <- gets (Map.lookup term . unCache)
  case cached :: Maybe (Set.Set value) of
    Just results -> foldMapA pure results
    Nothing -> do
      results <- asks (fromMaybe mempty . Map.lookup term . unCache)
      modify (Cache . Map.insert term (results :: Set.Set value) . unCache)
      result <- eval term
      result <$ modify (Cache . Map.insertWith (<>) term (Set.singleton (result :: value)) . unCache)

runHeap :: StateC (Heap value) m a -> m (Heap value, a)
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
