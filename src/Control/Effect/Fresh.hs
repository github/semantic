{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.Fresh where

import Control.Effect
import Control.Monad.Effect.Internal

-- | An effect offering a (resettable) sequence of always-incrementing, and therefore “fresh,” type variables.
data Fresh a where
  -- | Request a reset of the sequence of variable names.
  Reset :: Int -> Fresh ()
  -- | Request a fresh variable name.
  Fresh :: Fresh Int

-- | Get a fresh variable name, guaranteed unused (since the last 'reset').
fresh :: (Effectful m, Member Fresh effects) => m effects Int
fresh = raise (send Fresh)

-- | Reset the sequence of variable names. Useful to avoid complicated alpha-equivalence comparisons when iteratively recomputing the results of an analysis til convergence.
reset :: (Effectful m, Member Fresh effects) => Int -> m effects ()
reset = raise . send . Reset


-- | 'Fresh' effects are interpreted starting from 0, incrementing the current name with each request for a fresh name, and overwriting the counter on reset.
instance RunEffect Fresh a where
  runEffect = relayState (0 :: Int) (const pure) (\ s action k -> case action of
    Fresh -> k (succ s) s
    Reset s' -> k s' ())
