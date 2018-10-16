{-# LANGUAGE ExistentialQuantification, TypeOperators, RankNTypes, UndecidableInstances #-}
module Semantic.Timeout
( timeout
, Timeout
, runTimeout
, TimeoutC(..)
, Duration(..)
) where

import           Control.Effect
import           Control.Monad.IO.Class
import           Data.Duration
import qualified System.Timeout as System

-- | Run an action with a timeout. Returns 'Nothing' when no result is available
-- within the specified duration. Uses 'System.Timeout.timeout' so all caveats
-- about not operating over FFI boundaries apply.
timeout :: (Member Timeout sig, Carrier sig m) => Duration -> m output -> m (Maybe output)
timeout n = send . flip (Timeout n) gen

-- | 'Timeout' effects run other effects, aborting them if they exceed the
-- specified duration.
data Timeout m k
  = forall a . Timeout Duration (m a) (Maybe a -> k)

deriving instance Functor (Timeout m)

instance HFunctor Timeout where
  hmap f (Timeout n task k) = Timeout n (f task) k

instance Effect Timeout where
  handle state handler (Timeout n task k) = Timeout n (handler (task <$ state)) (handler . maybe (k Nothing <$ state) (fmap (k . Just)))

-- | Evaulate a 'Timeoute' effect.
runTimeout :: (Carrier sig m, MonadIO m)
           => (forall x . m x -> IO x)
           -> Eff (TimeoutC m) a
           -> m a
runTimeout handler = runTimeoutC handler . interpret
-- runTimeout handler = interpret (\ (Timeout n task) -> liftIO (System.timeout (toMicroseconds n) (handler (runTimeout handler task))))

newtype TimeoutC m a = TimeoutC ((forall x . m x -> IO x) -> m a)

runTimeoutC :: (forall x . m x -> IO x) -> TimeoutC m a -> m a
runTimeoutC f (TimeoutC m) = m f

instance (Carrier sig m, MonadIO m) => Carrier (Timeout :+: sig) (TimeoutC m) where
  gen a = TimeoutC (const (gen a))
  alg op = TimeoutC (\ handler ->
    ((\ (Timeout n task k) -> liftIO (System.timeout (toMicroseconds n) (handler (runTimeoutC handler task))) >>= runTimeoutC handler . k)
    \/ (alg . handlePure (runTimeoutC handler)))
      op)
