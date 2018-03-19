{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.Fresh where

import Control.Effect
import Control.Monad.Effect.Internal

type TName = Int

-- | An effect offering a (resettable) sequence of always-incrementing, and therefore “fresh,” type variables.
data Fresh a where
  -- | Request a reset of the sequence of variable names.
  Reset :: TName -> Fresh ()
  -- | Request a fresh variable name.
  Fresh :: Fresh TName

-- | 'Monad's offering a (resettable) sequence of guaranteed-fresh type variables.
class Monad m => MonadFresh m where
  -- | Get a fresh variable name, guaranteed unused (since the last 'reset').
  fresh :: m TName

  -- | Reset the sequence of variable names. Useful to avoid complicated alpha-equivalence comparisons when iteratively recomputing the results of an analysis til convergence.
  reset :: TName -> m ()

instance (Fresh :< fs) => MonadFresh (Eff fs) where
  fresh = send Fresh
  reset = send . Reset


-- | 'Fresh' effects are interpreted starting from 0, incrementing the current name with each request for a fresh name, and overwriting the counter on reset.
instance RunEffect Fresh a where
  runEffect = relayState (0 :: TName) (const pure) (\ s action k -> case action of
    Fresh -> k (succ s) s
    Reset s' -> k s' ())
