{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- TODO: We should kill this entirely, because with fused-effects 1.0 we can unlift the various runConcurrently operations.
module Semantic.Distribute
( distribute
, distributeFor
, distributeFoldMap
, Distribute
, runDistribute
, withDistribute
, DistributeC(..)
) where

import           Control.Algebra
import           Control.Carrier.Lift
import           Control.Carrier.Reader
import qualified Control.Concurrent.Async as Async
import           Control.Monad.IO.Unlift
import           Control.Parallel.Strategies
import           Data.Foldable (fold)

-- | Distribute a 'Traversable' container of tasks over the available cores (i.e. execute them concurrently), collecting their results.
--
--   This is a concurrent analogue of 'sequenceA'.
distribute :: (Has Distribute sig m, Traversable t) => t (m output) -> m (t output)
distribute = fmap (withStrategy (parTraversable rseq)) <$> traverse (send . flip Distribute pure)

-- | Distribute the application of a function to each element of a 'Traversable' container of inputs over the available cores (i.e. perform the function concurrently for each element), collecting the results.
--
--   This is a concurrent analogue of 'for' or 'traverse' (with the arguments flipped).
distributeFor :: (Has Distribute sig m, Traversable t) => t a -> (a -> m output) -> m (t output)
distributeFor inputs toTask = distribute (fmap toTask inputs)

-- | Distribute the application of a function to each element of a 'Traversable' container of inputs over the available cores (i.e. perform the function concurrently for each element), combining the results 'Monoid'ally into a final value.
--
--   This is a concurrent analogue of 'foldMap'.
distributeFoldMap :: (Has Distribute sig m, Monoid output, Traversable t) => (a -> m output) -> t a -> m output
distributeFoldMap toTask inputs = fmap fold (distribute (fmap toTask inputs))


-- | Distribute effects run tasks concurrently.
data Distribute m k
  = forall a . Distribute (m a) (a -> m k)

deriving instance Functor m => Functor (Distribute m)

instance HFunctor Distribute where
  hmap f (Distribute m k) = Distribute (f m) (f . k)

instance Effect Distribute where
  thread state handler (Distribute task k) = Distribute (handler (task <$ state)) (handler . fmap k)


-- | Evaluate a 'Distribute' effect concurrently.
runDistribute :: UnliftIO m -> DistributeC m a -> IO a
runDistribute u@(UnliftIO unlift) = unlift . runReader u . runDistributeC

withDistribute :: MonadUnliftIO m => DistributeC m a -> m a
withDistribute r = withUnliftIO (`runDistribute` r)

instance MonadUnliftIO m => MonadUnliftIO (LiftC m) where
  askUnliftIO = LiftC $ withUnliftIO $ \u -> pure (UnliftIO (unliftIO u . runM))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = LiftC $ withRunInIO $ \run -> inner (run . runM)
  {-# INLINE withRunInIO #-}

newtype DistributeC m a = DistributeC { runDistributeC :: ReaderC (UnliftIO m) m a }
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO)

-- This can be simpler if we add an instance to fused-effects that takes
-- care of this folderol for us (then we can justt derive the MonadUnliftIO instance)
instance (MonadIO m, Algebra sig m) => MonadUnliftIO (DistributeC m) where
  askUnliftIO = DistributeC . ReaderC $ \ u -> pure (UnliftIO (runDistribute u))

instance (Algebra sig m, MonadIO m) => Algebra (Distribute :+: sig) (DistributeC m) where
  alg (L (Distribute task k)) = do
    handler <- DistributeC ask
    liftIO (Async.runConcurrently (Async.Concurrently (runDistribute handler task))) >>= k
  alg (R other) = DistributeC (alg (R (handleCoercible other)))
