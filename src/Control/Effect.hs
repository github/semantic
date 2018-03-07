{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Effect where

import qualified Control.Monad.Effect as Effect
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Internal hiding (run)
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Control.Monad.Effect.Writer
import Data.Semigroup.Reducer
import Prologue

-- | Run a computation in 'Eff' to completion, interpreting each effect with some sensible defaults, and return the 'Final' result.
run :: RunEffects fs a => Eff fs a -> Final fs a
run = Effect.run . runEffects

-- | A typeclass to run a computation to completion, interpreting each effect with some sensible defaults.
class RunEffects fs a where
  -- | The final result type of the computation, factoring in the results of any effects, e.g. pairing 'State' results with the final state, wrapping 'Fail' results in 'Either', etc.
  type Final fs a
  runEffects :: Eff fs a -> Eff '[] (Final fs a)

instance (RunEffect f1 a, RunEffects (f2 ': fs) (Result f1 a)) => RunEffects (f1 ': f2 ': fs) a where
  type Final (f1 ': f2 ': fs) a = Final (f2 ': fs) (Result f1 a)
  runEffects = runEffects . runEffect

instance RunEffect f a => RunEffects '[f] a where
  type Final '[f] a = Result f a
  runEffects = runEffect


-- | A typeclass to interpret a single effect with some sensible defaults (defined per-effect).
class RunEffect f a where
  -- | The incremental result of an effect w.r.t. the parameter value, factoring in the interpretation of the effect.
  type Result f a
  type instance Result f a = a

  -- | Interpret the topmost effect in a computation with some sensible defaults (defined per-effect), and return the incremental 'Result'.
  runEffect :: Eff (f ': fs) a -> Eff fs (Result f a)

-- | 'State' effects with 'Monoid'al states are interpreted starting from the 'mempty' state value into a pair of result value and final state.
instance Monoid b => RunEffect (State b) a where
  type Result (State b) a = (a, b)
  runEffect = flip runState mempty

-- | 'Reader' effects with 'Monoid'al environments are interpreted starting from the 'mempty' environment value.
instance Monoid b => RunEffect (Reader b) a where
  runEffect = flip runReader mempty

-- | 'Fail' effects are interpreted into 'Either' s.t. failures are in 'Left' and successful results are in 'Right'.
instance RunEffect Fail a where
  type Result Fail a = Either String a
  runEffect = runFail

-- | 'Writer' effects are interpreted into a pair of result value and final log.
instance Monoid w => RunEffect (Writer w) a where
  type Result (Writer w) a = (a, w)
  runEffect = runWriter

-- | 'NonDetEff' effects are interpreted into a nondeterministic set of result values.
instance Ord a => RunEffect NonDetEff a where
  type Result NonDetEff a = Set a
  runEffect = relay (pure . unit) (\ m k -> case m of
    MZero -> pure mempty
    MPlus -> mappend <$> k True <*> k False)


class LiftEffect f where
  lift :: Eff effects a -> f effects a
  lower :: f effects a -> Eff effects a

instance LiftEffect Eff where
  lift = id
  lower = id
