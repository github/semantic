{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Monad.Effect.NonDet
( MonadNonDet(..)
, NonDetEff
) where

import Control.Monad.Effect.Internal
import Control.Monad.Effect.NonDetEff
import Data.Semigroup.Reducer
import Prologue

-- | 'Monad's offering local isolation of nondeterminism effects.
class (Alternative m, Monad m) => MonadNonDet m where
  -- | Run a computation, gathering any nondeterministically produced results into a single 'Monoid'al value.
  gather :: (Monoid b, Reducer a b)
         => m a -- ^ The computation to run locally-nondeterministically.
         -> m b -- ^ A _deterministic_ computation producing the 'Monoid'al accumulation of the _locally-nondeterministic_ result values.

-- | Effect stacks containing 'NonDetEff' offer a 'MonadNonDet' instance which implements 'gather' by interpreting the requests for nondeterminism locally, without removing 'NonDetEff' from the stack—i.e. the _capacity_ for nondeterminism is still present in the effect stack, but any local nondeterminism has been applied.
instance (NonDetEff :< fs) => MonadNonDet (Eff fs) where
  gather = interpose (pure . unit) (\ m k -> case m of
    MZero -> pure mempty
    MPlus -> mappend <$> k True <*> k False)
