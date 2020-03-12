{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Control.Effect.Timeout
( timeout
) where

import           Control.Algebra
import           Control.Effect.Lift
import qualified System.Timeout as System

-- | Run an action with a timeout. Returns 'Nothing' when no result is available
-- within the specified duration. Uses 'System.Timeout.timeout' so all caveats
-- about not operating over FFI boundaries apply.
--
-- Any state changes in the action are discarded iff the timeout fails.
timeout :: Has (Lift IO) sig m => Int -> m a -> m (Maybe a)
timeout n m = liftWith $ \ ctx hdl
  -> maybe
     -- Restore the old state if it timed out.
     (Nothing <$ ctx)
     -- Apply it if it succeeded.
     (fmap Just) <$> System.timeout n (hdl (m <$ ctx))
