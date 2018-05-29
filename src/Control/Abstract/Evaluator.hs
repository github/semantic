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
import Control.Monad.Effect.Fresh     as X
import Control.Monad.Effect.NonDet    as X
import Control.Monad.Effect.Reader    as X
import Control.Monad.Effect.Resumable as X
import Control.Monad.Effect.State     as X
import Control.Monad.Effect.Trace     as X
import Data.Abstract.Address
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
data Return location value resume where
  Return :: Address location value -> Return location value (Address location value)

deriving instance (Eq location, Eq value) => Eq (Return location value a)
deriving instance (Show location, Eq value) => Show (Return location value a)

earlyReturn :: Member (Return location value) effects => Address location value -> Evaluator location value effects (Address location value)
earlyReturn = send . Return

catchReturn :: Member (Return location value) effects => Evaluator location value effects a -> (forall x . Return location value x -> Evaluator location value effects a) -> Evaluator location value effects a
catchReturn action handler = interpose pure (\ ret _ -> handler ret) action

runReturn :: Evaluator location value (Return location value ': effects) (Address location value) -> Evaluator location value effects (Address location value)
runReturn = relay pure (\ (Return value) _ -> pure value)


-- | Effects for control flow around loops (breaking and continuing).
data LoopControl location value resume where
  Break    :: Address location value -> LoopControl location value (Address location value)
  Continue :: Address location value -> LoopControl location value (Address location value)

deriving instance (Eq location, Eq value) => Eq (LoopControl location value a)
deriving instance (Show location, Show value) => Show (LoopControl location value a)

throwBreak :: Member (LoopControl location value) effects
           => Address location value
           -> Evaluator location value effects (Address location value)
throwBreak = send . Break

throwContinue :: Member (LoopControl location value) effects
              => Address location value
              -> Evaluator location value effects (Address location value)
throwContinue = send . Continue

catchLoopControl :: Member (LoopControl location value) effects
                 => Evaluator location value effects a
                 -> (forall x . LoopControl location value x -> Evaluator location value effects a)
                 -> Evaluator location value effects a
catchLoopControl action handler = interpose pure (\ control _ -> handler control) action

runLoopControl :: Evaluator location value (LoopControl location value ': effects) (Address location value)
               -> Evaluator location value effects (Address location value)
runLoopControl = relay pure (\ eff _ -> case eff of
  Break    value -> pure value
  Continue value -> pure value)
