{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Control.Abstract.Evaluator
  ( Evaluator(..)
  , Open
  -- * Effects
  , Return(..)
  , earlyReturn
  , catchReturn
  , runReturn
  , LoopControl(..)
  , throwBreak
  , throwContinue
  , throwAbort
  , catchLoopControl
  , runLoopControl
  , module X
  ) where

import Control.Effect           as X
import Control.Effect.Carrier
import Control.Effect.Error     as X
import Control.Effect.Fresh     as X
import Control.Effect.NonDet    as X
import Control.Effect.Reader    as X
import Control.Effect.Resumable as X
import Control.Effect.State     as X
import Control.Effect.Trace     as X
import Control.Monad.IO.Class

-- | An 'Evaluator' is a thin wrapper around 'Eff' with (phantom) type parameters for the address, term, and value types.
--
--   These parameters enable us to constrain the types of effects using them s.t. we can avoid both ambiguous types when they aren’t mentioned outside of the context, and lengthy, redundant annotations on the use sites of functions employing these effects.
--
--   These effects will typically include the environment, heap, module table, etc. effects necessary for evaluation of modules and terms, but may also include any other effects so long as they’re eventually handled.
newtype Evaluator term address value m a = Evaluator { runEvaluator :: Eff m a }
  deriving (Applicative, Functor, Monad)

deriving instance (Member NonDet sig, Carrier sig m) => Alternative (Evaluator term address value m)
deriving instance (Member (Lift IO) sig, Carrier sig m) => MonadIO (Evaluator term address value m)

instance Carrier sig m => Carrier sig (Evaluator term address value m) where
  ret = Evaluator . ret
  eff = Evaluator . eff . handlePure runEvaluator


-- | An open-recursive function.
type Open a = a -> a


-- Effects

-- | An effect for explicitly returning out of a function/method body.
newtype Return address = Return { unReturn :: address }
  deriving (Eq, Ord, Show)

earlyReturn :: (Member (Error (Return address)) sig, Carrier sig m)
            => address
            -> Evaluator term address value m address
earlyReturn = throwError . Return

catchReturn :: (Member (Error (Return address)) sig, Carrier sig m) => Evaluator term address value m address -> Evaluator term address value m address
catchReturn = flip catchError (\ (Return addr) -> pure addr)

runReturn :: (Carrier sig m, Effect sig) => Evaluator term address value (ErrorC (Return address) (Evaluator term address value m)) address -> Evaluator term address value m address
runReturn = fmap (either unReturn id) . runError . runEvaluator


-- | Effects for control flow around loops (breaking and continuing).
data LoopControl address
  = Break    { unLoopControl :: address }
  | Continue { unLoopControl :: address }
  | Abort
  deriving (Eq, Ord, Show)

throwBreak :: (Member (Error (LoopControl address)) sig, Carrier sig m)
           => address
           -> Evaluator term address value m address
throwBreak = throwError . Break

throwContinue :: (Member (Error (LoopControl address)) sig, Carrier sig m)
              => address
              -> Evaluator term address value m address
throwContinue = throwError . Continue

throwAbort :: forall term address sig m value a
           .  (Member (Error (LoopControl address)) sig, Carrier sig m)
           => Evaluator term address value m a
throwAbort = throwError (Abort @address)

catchLoopControl :: (Member (Error (LoopControl address)) sig, Carrier sig m) => Evaluator term address value m a -> (LoopControl address -> Evaluator term address value m a) -> Evaluator term address value m a
catchLoopControl = catchError

runLoopControl :: (Carrier sig m, Effect sig) => Evaluator term address value (ErrorC (LoopControl address) (Evaluator term address value m)) address -> Evaluator term address value m address
runLoopControl = fmap (either unLoopControl id) . runError . runEvaluator
