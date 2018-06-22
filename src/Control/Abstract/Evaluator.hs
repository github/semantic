{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures, RankNTypes, ScopedTypeVariables, TypeOperators #-}
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
import Control.Monad.Effect.Exception as Exc
import Control.Monad.Effect.Fresh     as X
import qualified Control.Monad.Effect.Internal as Eff
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
data Return address (m :: * -> *) resume where
  Return :: address -> Return address m address

deriving instance Eq address   => Eq   (Return address m a)
deriving instance Show address => Show (Return address m a)

instance Effect (Return address) where
  handleState c dist (Request (Return value) k) = Request (Return value) (dist . (<$ c) . k)

earlyReturn :: Member (Return address) effects
            => address
            -> Evaluator address value effects address
earlyReturn = send . Return

catchReturn :: forall m address value effects . (Member (Return address) effects, Effectful (m address value)) => m address value effects address -> m address value effects address
catchReturn = Eff.raiseHandler (interpose @(Return address) pure (\ (Return ret) _ -> pure ret))

runReturn :: (Effectful (m address value), Effects effects) => m address value (Return address ': effects) address -> m address value effects address
runReturn = Eff.raiseHandler go . catchReturn
  where go :: Effects effects => Eff (Return address ': effects) a -> Eff effects a
        go (Eff.Return a) = pure a
        go (Effect (Return a) k) = go (k a)
        go (Other u k) = liftHandler go u k


-- | Effects for control flow around loops (breaking and continuing).
data LoopControl address (m :: * -> *) resume where
  Break    :: address -> LoopControl address m address
  Continue :: address -> LoopControl address m address

deriving instance Eq address   => Eq   (LoopControl address m a)
deriving instance Show address => Show (LoopControl address m a)

instance Effect (LoopControl address) where
  handleState c dist (Request (Break value) k) = Request (Break value) (dist . (<$ c) . k)
  handleState c dist (Request (Continue value) k) = Request (Continue value) (dist . (<$ c) . k)

throwBreak :: Member (LoopControl address) effects
           => address
           -> Evaluator address value effects address
throwBreak = send . Break

throwContinue :: Member (LoopControl address) effects
              => address
              -> Evaluator address value effects address
throwContinue = send . Continue

catchLoopControl :: Member (LoopControl address) effects => Evaluator address value effects a -> (forall x . LoopControl address (Eff effects) x -> Evaluator address value effects a) -> Evaluator address value effects a
catchLoopControl action handler = interpose pure (\ control _ -> handler control) action

runLoopControl :: (Effectful (m address value), Effects effects) => m address value (LoopControl address ': effects) address -> m address value effects address
runLoopControl = Eff.raiseHandler (fmap (either id id) . Exc.runError . reinterpret (\ eff -> case eff of
  Break    value -> Exc.throwError value
  Continue value -> Exc.throwError value))
