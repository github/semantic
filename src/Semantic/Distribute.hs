{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Semantic.Distribute
( distribute
, distributeFor
, distributeFoldMap
, Distribute
, runDistribute
, withDistribute
, DistributeC(..)
) where

import qualified Control.Concurrent.Async as Async
import           Control.Effect.Carrier
import           Control.Effect.Reader
import           Control.Monad.IO.Unlift
import           Control.Parallel.Strategies
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
  = forall a . Distribute (m a) (a -> m k)

deriving instance Functor m => Functor (Distribute m)

instance HFunctor Distribute where
  hmap f (Distribute m k) = Distribute (f m) (f . k)

instance Effect Distribute where
  handle state handler (Distribute task k) = Distribute (handler (task <$ state)) (handler . fmap k)


-- | Evaluate a 'Distribute' effect concurrently.
runDistribute :: UnliftIO m -> DistributeC m a -> IO a
runDistribute u@(UnliftIO unlift) = unlift . runReader u . runDistributeC

withDistribute :: MonadUnliftIO m => DistributeC m a -> m a
withDistribute r = withUnliftIO (`runDistribute` r)

newtype DistributeC m a = DistributeC { runDistributeC :: ReaderC (UnliftIO m) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- This can be simpler if we add an instance to fused-effects that takes
-- care of this folderol for us (then we can justt derive the MonadUnliftIO instance)
instance (MonadIO m, Carrier sig m) => MonadUnliftIO (DistributeC m) where
  askUnliftIO = DistributeC . ReaderC $ \ u -> pure (UnliftIO (runDistribute u))

instance (Carrier sig m, MonadIO m) => Carrier (Distribute :+: sig) (DistributeC m) where
  eff (L (Distribute task k)) = do
    handler <- DistributeC ask
    liftIO (Async.runConcurrently (Async.Concurrently (runDistribute handler task))) >>= k
  eff (R other) = DistributeC (eff (R (handleCoercible other)))
