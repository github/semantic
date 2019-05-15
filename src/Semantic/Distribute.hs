{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, TypeOperators, UndecidableInstances #-}
module Semantic.Distribute
( distribute
, distributeFor
, distributeFoldMap
, Distribute
, runDistribute
, DistributeC(..)
) where

import qualified Control.Concurrent.Async as Async
import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Sum
import           Control.Parallel.Strategies
import           Control.Monad.IO.Unlift
import           Prologue

-- | Distribute a 'Traversable' container of tasks over the available cores (i.e. execute them concurrently), collecting their results.
--
--   This is a concurrent analogue of 'sequenceA'.
distribute :: (Member Distribute sig, Traversable t, Carrier sig m) => t (m output) -> m (t output)
distribute = fmap (withStrategy (parTraversable rseq)) <$> traverse (send . flip Distribute pure)

-- | Distribute the application of a function to each element of a 'Traversable' container of inputs over the available cores (i.e. perform the function concurrently for each element), collecting the results.
--
--   This is a concurrent analogue of 'for' or 'traverse' (with the arguments flipped).
distributeFor :: (Member Distribute sig, Traversable t, Carrier sig m) => t a -> (a -> m output) -> m (t output)
distributeFor inputs toTask = distribute (fmap toTask inputs)

-- | Distribute the application of a function to each element of a 'Traversable' container of inputs over the available cores (i.e. perform the function concurrently for each element), combining the results 'Monoid'ally into a final value.
--
--   This is a concurrent analogue of 'foldMap'.
distributeFoldMap :: (Member Distribute sig, Monoid output, Traversable t, Carrier sig m) => (a -> m output) -> t a -> m output
distributeFoldMap toTask inputs = fmap fold (distribute (fmap toTask inputs))


-- | Distribute effects run tasks concurrently.
data Distribute m k
  = forall a . Distribute (m a) (a -> k)

deriving instance Functor (Distribute m)

instance HFunctor Distribute where
  hmap f (Distribute m k) = Distribute (f m) k

instance Effect Distribute where
  handle state handler (Distribute task k) = Distribute (handler (task <$ state)) (handler . fmap k)


-- | Evaluate a 'Distribute' effect concurrently.
runDistribute :: DistributeC (LiftC IO) a -> LiftC IO a
runDistribute = runDistributeC

newtype DistributeC m a = DistributeC { runDistributeC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Carrier (Distribute :+: Lift IO) (DistributeC (LiftC IO)) where
  eff (L (Distribute task k)) = liftIO (Async.runConcurrently (Async.Concurrently (runM . runDistributeC $ task))) >>= k
  eff (R other) = DistributeC (eff (handleCoercible other))

instance MonadUnliftIO m => MonadUnliftIO (DistributeC m) where
  askUnliftIO = DistributeC $ withUnliftIO $ \u -> pure (UnliftIO (unliftIO u . runDistributeC))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = DistributeC $ withRunInIO $ \run -> inner (run . runDistributeC)
  {-# INLINE withRunInIO #-}
