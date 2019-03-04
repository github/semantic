{-# LANGUAGE ExistentialQuantification, TypeOperators, RankNTypes, UndecidableInstances #-}
module Semantic.Timeout
( timeout
, Timeout
, runTimeout
, TimeoutC(..)
, Duration(..)
) where

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Sum
import           Control.Monad.IO.Class
import           Data.Duration
import qualified System.Timeout as System

-- | Run an action with a timeout. Returns 'Nothing' when no result is available
-- within the specified duration. Uses 'System.Timeout.timeout' so all caveats
-- about not operating over FFI boundaries apply.
timeout :: (Member Timeout sig, Carrier sig m) => Duration -> m output -> m (Maybe output)
timeout n = send . flip (Timeout n) ret

-- | 'Timeout' effects run other effects, aborting them if they exceed the
-- specified duration.
data Timeout m k
  = forall a . Timeout Duration (m a) (Maybe a -> k)

deriving instance Functor (Timeout m)

instance HFunctor Timeout where
  hmap f (Timeout n task k) = Timeout n (f task) k

instance Effect Timeout where
  handle state handler (Timeout n task k) = Timeout n (handler (task <$ state)) (handler . maybe (k Nothing <$ state) (fmap (k . Just)))

-- | Evaulate a 'Timeout' effect.
runTimeout :: (forall x . m x -> IO x)
           -> TimeoutC m a
           -> m a
runTimeout handler = runReader (Handler handler) . runTimeoutC

newtype Handler m = Handler (forall x . m x -> IO x)

runHandler :: Handler m -> TimeoutC m a -> IO a
runHandler h@(Handler handler) = handler . runReader h . runTimeoutC

newtype TimeoutC m a = TimeoutC { runTimeoutC :: ReaderC (Handler m) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Carrier sig m, MonadIO m) => Carrier (Timeout :+: sig) (TimeoutC m) where
  eff (L (Timeout n task k)) = do
    handler <- TimeoutC ask
    liftIO (System.timeout (toMicroseconds n) (runHandler handler task)) >>= k
  eff (R other) = TimeoutC (eff (R (handleCoercible other)))
