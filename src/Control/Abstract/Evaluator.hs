{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
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
  , module Control.Effect
  , module Control.Monad.Effect.Fail
  , module Control.Monad.Effect.Fresh
  , module Control.Monad.Effect.NonDet
  , module Control.Monad.Effect.Reader
  , module Control.Monad.Effect.Resumable
  , module Control.Monad.Effect.State
  , relay
  ) where

import Control.Effect
import Control.Monad.Effect (Eff, interpose, relay)
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.NonDet
import Control.Monad.Effect.Reader hiding (runReader)
import Control.Monad.Effect.Resumable
import Control.Monad.Effect.State hiding (runState)
import Prologue

-- | An 'Evaluator' is a thin wrapper around 'Eff' with (phantom) type parameters for the location, term, and value types.
--
--   These parameters enable us to constrain the types of effects using them s.t. we can avoid both ambiguous types when they aren’t mentioned outside of the context, and lengthy, redundant annotations on the use sites of functions employing these effects.
--
--   These effects will typically include the environment, heap, module table, etc. effects necessary for evaluation of modules and terms, but may also include any other effects so long as they’re eventually handled.
newtype Evaluator location value effects a = Evaluator { runEvaluator :: Eff effects a }
  deriving (Applicative, Effectful, Functor, Monad)

deriving instance Member NonDet effects => Alternative (Evaluator location value effects)


-- Effects

-- | An effect for explicitly returning out of a function/method body.
data Return value resume where
  Return :: value -> Return value value

deriving instance Eq value => Eq (Return value a)
deriving instance Show value => Show (Return value a)

earlyReturn :: Member (Return value) effects => value -> Evaluator location value effects value
earlyReturn = send . Return

catchReturn :: Member (Return value) effects => Evaluator location value effects a -> (forall x . Return value x -> Evaluator location value effects a) -> Evaluator location value effects a
catchReturn action handler = raiseHandler (interpose pure (\ ret _ -> lower (handler ret))) action

runReturn :: Evaluator location value (Return value ': effects) value -> Evaluator location value effects value
runReturn = runEffect (\ (Return value) _ -> pure value)


-- | Effects for control flow around loops (breaking and continuing).
data LoopControl value resume where
  Break    :: value -> LoopControl value value
  Continue :: value -> LoopControl value value

deriving instance Eq value => Eq (LoopControl value a)
deriving instance Show value => Show (LoopControl value a)

throwBreak :: Member (LoopControl value) effects => value -> Evaluator location value effects value
throwBreak = send . Break

throwContinue :: Member (LoopControl value) effects => value -> Evaluator location value effects value
throwContinue = send . Continue

catchLoopControl :: Member (LoopControl value) effects => Evaluator location value effects a -> (forall x . LoopControl value x -> Evaluator location value effects a) -> Evaluator location value effects a
catchLoopControl action handler = raiseHandler (interpose pure (\ control _ -> lower (handler control))) action

runLoopControl :: Evaluator location value (LoopControl value ': effects) value -> Evaluator location value effects value
runLoopControl = runEffect (\ eff _ -> case eff of
  Break    value -> pure value
  Continue value -> pure value)
