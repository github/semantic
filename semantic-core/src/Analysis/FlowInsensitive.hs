{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
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
import qualified Data.Core as Core
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Alt(..))
import qualified Data.Set as Set
import           Data.Term (Term)

type Cache name a = Map.Map (Term Core.Core name) (Set.Set a)
type Heap name a = Map.Map name (Set.Set a)

newtype FrameId name = FrameId { unFrameId :: name }
  deriving (Eq, Ord, Show)


convergeTerm :: forall m sig a name
             .  ( Carrier sig m
                , Effect sig
                , Member Fresh sig
                , Member (State (Heap name a)) sig
                , Ord a
                , Ord name
                )
             => (Term Core.Core name -> NonDetC (ReaderC (Cache name a) (StateC (Cache name a) m)) a)
             -> Term Core.Core name
             -> m (Set.Set a)
convergeTerm eval body = do
  heap <- get
  (cache, _) <- converge (Map.empty :: Cache name a, heap :: Heap name a) $ \ (prevCache, _) -> runState Map.empty . runReader prevCache $ do
    _ <- resetFresh . runNonDetM Set.singleton $ eval body
    get
  pure (fromMaybe mempty (Map.lookup body cache))

cacheTerm :: forall m sig a name
          .  ( Alternative m
             , Carrier sig m
             , Member (Reader (Cache name a)) sig
             , Member (State  (Cache name a)) sig
             , Ord a
             , Ord name
             )
          => (Term Core.Core name -> m a)
          -> (Term Core.Core name -> m a)
cacheTerm eval term = do
  cached <- gets (Map.lookup term)
  case cached :: Maybe (Set.Set a) of
    Just results -> foldMapA pure results
    Nothing -> do
      results <- asks (fromMaybe mempty . Map.lookup term)
      modify (Map.insert term (results :: Set.Set a))
      result <- eval term
      result <$ modify (Map.insertWith (<>) term (Set.singleton (result :: a)))

runHeap :: name -> ReaderC (FrameId name) (StateC (Heap name a) m) b -> m (Heap name a, b)
runHeap addr m = runState (Map.singleton addr Set.empty) (runReader (FrameId addr) m)

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
