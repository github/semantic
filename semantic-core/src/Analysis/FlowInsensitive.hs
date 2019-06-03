{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
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
import           Data.Name
import qualified Data.Set as Set
import qualified Semantic.Core as Core

type Cache a = Map.Map Core.Core (Set.Set a)
type Heap a = Map.Map Name (Set.Set a)

newtype FrameId = FrameId { unFrameId :: Name }
  deriving (Eq, Ord, Show)


convergeTerm :: forall m sig a
             .  ( Carrier sig m
                , Effect sig
                , Member Fresh sig
                , Member (State (Heap a)) sig
                , Ord a
                )
             => (Core.Core -> NonDetC (ReaderC (Cache a) (StateC (Cache a) m)) a)
             -> Core.Core
             -> m (Set.Set a)
convergeTerm eval body = do
  heap <- get
  (cache, _) <- converge (Map.empty :: Cache a, heap :: Heap a) $ \ (prevCache, _) -> runState Map.empty . runReader prevCache $ do
    _ <- resetFresh . runNonDetM Set.singleton $ eval body
    get
  pure (fromMaybe mempty (Map.lookup body cache))

cacheTerm :: forall m sig a
          .  ( Alternative m
             , Carrier sig m
             , Member (Reader (Cache a)) sig
             , Member (State  (Cache a)) sig
             , Ord a
             )
          => (Core.Core -> m a)
          -> (Core.Core -> m a)
cacheTerm eval term = do
  cached <- gets (Map.lookup term)
  case cached :: Maybe (Set.Set a) of
    Just results -> foldMapA pure results
    Nothing -> do
      results <- asks (fromMaybe mempty . Map.lookup term)
      modify (Map.insert term (results :: Set.Set a))
      result <- eval term
      result <$ modify (Map.insertWith (<>) term (Set.singleton (result :: a)))

runHeap :: (Carrier sig m, Member Naming sig) => ReaderC FrameId (StateC (Heap a) m) b -> m (Heap a, b)
runHeap m = do
  addr <- Gen <$> gensym "root"
  runState (Map.singleton addr Set.empty) (runReader (FrameId addr) m)

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
