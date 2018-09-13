{-# LANGUAGE TypeOperators, GADTs, RankNTypes #-}
module Semantic.Timeout
( timeout
, Timeout
, runTimeout
, Duration(..)
) where

import           Control.Monad.Effect
import           Control.Monad.IO.Class
import           Data.Duration
import qualified System.Timeout as System

-- | Run an action with a timeout. Returns 'Nothing' when no result is available
-- within the specified duration. Uses 'System.Timeout.timeout' so all caveats
-- about not operating over FFI boundaries apply.
timeout :: (Member Timeout effs) => Duration -> Eff effs output -> Eff effs (Maybe output)
timeout n = send . Timeout n

-- | 'Timeout' effects run other effects, aborting them if they exceed the
-- specified duration.
data Timeout task output where
   Timeout :: Duration -> task output -> Timeout task (Maybe output)

instance PureEffect Timeout
instance Effect Timeout where
  handleState c dist (Request (Timeout n task) k) = Request (Timeout n (dist (task <$ c))) (dist . maybe (k Nothing <$ c) (fmap (k . Just)))

-- | Evaulate a 'Timeoute' effect.
runTimeout :: (Member (Lift IO) effects, PureEffects effects)
  => (forall x . Eff effects x -> IO x)
  -> Eff (Timeout ': effects) a
  -> Eff effects a
runTimeout handler = interpret (\ (Timeout n task) -> liftIO (System.timeout (toMicroseconds n) (handler (runTimeout handler task))))
