{-# LANGUAGE ScopedTypeVariables #-}
module Control.Effect.Timeout
( timeout
) where

import           Control.Algebra
import           Control.Effect.Lift
import           Data.Duration
import qualified System.Timeout as System

-- | Run an action with a timeout. Returns 'Nothing' when no result is available
-- within the specified duration. Uses 'System.Timeout.timeout' so all caveats
-- about not operating over FFI boundaries apply.
--
-- Any state changes in the action are discarded if the timeout fails.
timeout :: Has (Lift IO) sig m => Duration -> m a -> m (Maybe a)
timeout n m = liftWith $ \ hdl ctx
  -> maybe
     -- Restore the old state if it timed out.
     (Nothing <$ ctx)
     -- Apply it if it succeeded.
     (fmap Just) <$> System.timeout (toMicroseconds n) (hdl (m <$ ctx))
