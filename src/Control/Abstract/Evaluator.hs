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
  , module X
  ) where

import Control.Monad.Effect           as X
import Control.Monad.Effect.Fail      as X
import Control.Monad.Effect.Fresh     as X
import Control.Monad.Effect.NonDet    as X
import Control.Monad.Effect.Reader    as X
import Control.Monad.Effect.Resumable as X
import Control.Monad.Effect.State     as X
import Control.Monad.Effect.Trace     as X
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
catchReturn action handler = interpose pure (\ ret _ -> handler ret) action

runReturn :: Evaluator location value (Return value ': effects) value -> Evaluator location value effects value
runReturn = relay pure (\ (Return value) _ -> pure value)


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
catchLoopControl action handler = interpose pure (\ control _ -> handler control) action

runLoopControl :: Evaluator location value (LoopControl value ': effects) value -> Evaluator location value effects value
runLoopControl = relay pure (\ eff _ -> case eff of
  Break    value -> pure value
  Continue value -> pure value)
