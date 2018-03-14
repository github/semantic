{-# LANGUAGE DataKinds, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}
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

-- | Run an 'Effectful' computation to completion, interpreting each effect with some sensible defaults, and return the 'Final' result.
run :: (Effectful m, RunEffects effects a) => m effects a -> Final effects a
run = runEffects . lower

-- | A typeclass to run a computation to completion, interpreting each effect with some sensible defaults.
class RunEffects effects a where
  -- | The final result type of the computation, factoring in the results of any effects, e.g. pairing 'State' results with the final state, wrapping 'Fail' results in 'Either', etc.
  type Final effects a
  runEffects :: Eff effects a -> Final effects a

instance (RunEffect effect1 a, RunEffects effects (Result effect1 a)) => RunEffects (effect1 ': effects) a where
  type Final (effect1 ': effects) a = Final effects (Result effect1 a)
  runEffects = runEffects . runEffect

instance RunEffects '[] a where
  type Final '[] a = a
  runEffects = Effect.run


-- | A typeclass to interpret a single effect with some sensible defaults (defined per-effect).
class RunEffect effect a where
  -- | The incremental result of an effect w.r.t. the parameter value, factoring in the interpretation of the effect.
  type Result effect a
  type instance Result effect a = a

  -- | Interpret the topmost effect in a computation with some sensible defaults (defined per-effect), and return the incremental 'Result'.
  runEffect :: Eff (effect ': effects) a -> Eff effects (Result effect a)

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


-- | Types wrapping 'Eff' actions.
--
--   Most instances of 'Effectful' will be derived using @-XGeneralizedNewtypeDeriving@, with these ultimately bottoming out on the instance for 'Eff' (for which 'raise' and 'lower' are simply the identity). Because of this, types can be nested arbitrarily deeply and still call 'raise'/'lower' just once to get at the (ultimately) underlying 'Eff'.
class Effectful (m :: [* -> *] -> * -> *) where
  -- | Raise an action in 'Eff' into an action in @m@.
  raise :: Eff effects a -> m effects a
  -- | Lower an action in @m@ into an action in 'Eff'.
  lower :: m effects a -> Eff effects a

instance Effectful Eff where
  raise = id
  lower = id
