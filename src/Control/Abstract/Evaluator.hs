{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Control.Abstract.Evaluator
  ( Evaluator(..)
  -- * Effects
  , Return(..)
  , earlyReturn
  , catchReturn
  , runReturn
  , LoopControl(..)
  , throwBreak
  , throwContinue
  , catchLoopControl
  , runLoopControl
  , module X
  ) where

import Control.Monad.Effect           as X
import Control.Monad.Effect.Fresh     as X
import Control.Monad.Effect.Internal
import Control.Monad.Effect.NonDet    as X
import Control.Monad.Effect.Reader    as X
import Control.Monad.Effect.Resumable as X
import Control.Monad.Effect.State     as X
import Control.Monad.Effect.Trace     as X
import Prologue

-- | An 'Evaluator' is a thin wrapper around 'Eff' with (phantom) type parameters for the address, term, and value types.
--
--   These parameters enable us to constrain the types of effects using them s.t. we can avoid both ambiguous types when they aren’t mentioned outside of the context, and lengthy, redundant annotations on the use sites of functions employing these effects.
--
--   These effects will typically include the environment, heap, module table, etc. effects necessary for evaluation of modules and terms, but may also include any other effects so long as they’re eventually handled.
newtype Evaluator address value effects a = Evaluator { runEvaluator :: Eff effects a }
  deriving (Applicative, Effectful, Functor, Monad)

deriving instance Member NonDet effects => Alternative (Evaluator address value effects)

-- Effects

-- | An effect for explicitly returning out of a function/method body.
data Return address value resume where
  Return :: address -> Return address value address

deriving instance (Eq address, Eq value) => Eq (Return address value a)
deriving instance (Show address, Eq value) => Show (Return address value a)

earlyReturn :: forall address value effects
            .  Member (Return address value) effects
            => address
            -> Evaluator address value effects address
earlyReturn = send . Return @address @value

catchReturn :: Member (Return address value) effects => Evaluator address value effects a -> (forall x . Return address value x -> Evaluator address value effects a) -> Evaluator address value effects a
catchReturn action handler = interpose pure (\ ret _ -> handler ret) action

runReturn :: Effectful (m address value) => m address value (Return address value ': effects) address -> m address value effects address
runReturn = raiseHandler (relay pure (\ (Return value) _ -> pure value))


-- | Effects for control flow around loops (breaking and continuing).
data LoopControl address value resume where
  Break    :: address -> LoopControl address value address
  Continue :: address -> LoopControl address value address

deriving instance (Eq address, Eq value) => Eq (LoopControl address value a)
deriving instance (Show address, Show value) => Show (LoopControl address value a)

throwBreak :: forall address value effects
           .  Member (LoopControl address value) effects
           => address
           -> Evaluator address value effects address
throwBreak = send . Break @address @value

throwContinue :: forall address value effects
              .  Member (LoopControl address value) effects
              => address
              -> Evaluator address value effects address
throwContinue = send . Continue @address @value

catchLoopControl :: Member (LoopControl address value) effects => Evaluator address value effects a -> (forall x . LoopControl address value x -> Evaluator address value effects a) -> Evaluator address value effects a
catchLoopControl action handler = interpose pure (\ control _ -> handler control) action

runLoopControl :: Effectful (m address value) => m address value (LoopControl address value ': effects) address -> m address value effects address
runLoopControl = raiseHandler (relay pure (\ eff _ -> case eff of
  Break    value -> pure value
  Continue value -> pure value))
