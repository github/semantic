{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.Fresh where

import Control.Effect
import Control.Monad.Effect.Internal

type TName = Int

-- | An effect offering a (resettable) sequence of always-incrementing, and therefore “fresh,” type variables.
data Fresh a where
  Reset :: TName -> Fresh () -- ^ Reset the sequence of variable names. Useful to avoid complicated alpha-equivalence comparisons when iteratively recomputing the results of an analysis til convergence.
  Fresh :: Fresh TName       -- ^ Get a fresh variable name, guaranteed unused since the last reset.

class Monad m => MonadFresh m where
  fresh :: m TName
  reset :: TName -> m ()

instance (Fresh :< fs) => MonadFresh (Eff fs) where
  fresh = send Fresh
  reset = send . Reset


instance RunEffect Fresh a where
  runEffect = relayState (0 :: TName) (const pure) (\ s action k -> case action of
    Fresh -> k (succ s) s
    Reset s' -> k s' ())
