{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Control.Abstract.Evaluator
  ( Evaluator(..)
  , raiseHandler
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

import Control.Effect as X
import Control.Effect.Carrier
import Control.Effect.Error as X
import Control.Effect.Fresh as X
import Control.Effect.NonDet as X
import Control.Effect.Reader as X
import Control.Effect.Resumable as X
import Control.Effect.State as X
import Control.Effect.Trace as X
import Control.Monad.IO.Class
import Data.Abstract.Ref
import Data.Coerce

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


-- | Raise a handler on 'Eff's into a handler on 'Evaluator's.
raiseHandler :: (Eff m a -> Eff n b)
             -> Evaluator term address value m a
             -> Evaluator term address value n b
raiseHandler = coerce


-- | An open-recursive function.
type Open a = a -> a


-- Effects

-- | An effect for explicitly returning out of a function/method body.
newtype Return address value = Return { unReturn :: ValueRef address value }
  deriving (Eq, Ord, Show)

earlyReturn :: ( Member (Error (Return address value)) sig
               , Carrier sig m
               )
            => ValueRef address value
            -> Evaluator term address value m (ValueRef address value)
earlyReturn = throwError . Return

catchReturn :: (Member (Error (Return address value)) sig, Carrier sig m) => Evaluator term address value m (ValueRef address value) -> Evaluator term address value m (ValueRef address value)
catchReturn = flip catchError (\ (Return value) -> pure value)

runReturn :: (Carrier sig m, Effect sig) => Evaluator term address value (ErrorC (Return address value) (Eff m)) (ValueRef address value) -> Evaluator term address value m (ValueRef address value)
runReturn = raiseHandler $ fmap (either unReturn id) . runError


-- | Effects for control flow around loops (breaking and continuing).
data LoopControl address value
  = Break    { unLoopControl :: ValueRef address value }
  | Continue { unLoopControl :: ValueRef address value }
  | Abort
  deriving (Eq, Ord, Show)

throwBreak :: (Member (Error (LoopControl address value)) sig, Carrier sig m)
           => ValueRef address value
           -> Evaluator term address value m (ValueRef address value)
throwBreak = throwError . Break

throwContinue :: (Member (Error (LoopControl address value)) sig, Carrier sig m)
              => ValueRef address value
              -> Evaluator term address value m (ValueRef address value)
throwContinue = throwError . Continue

throwAbort :: forall term address sig m value a . (Member (Error (LoopControl address value)) sig , Carrier sig m)
           => Evaluator term address value m a
throwAbort = throwError (Abort @address @value)

catchLoopControl :: ( Member (Error (LoopControl address value)) sig
                    , Carrier sig m
                    )
                 => Evaluator term address value m a
                 -> (LoopControl address value -> Evaluator term address value m a)
                 -> Evaluator term address value m a
catchLoopControl = catchError

runLoopControl :: (Carrier sig m, Effect sig)
               => Evaluator term address value (ErrorC (LoopControl address value) (Eff m)) (ValueRef address value)
               -> Evaluator term address value m (ValueRef address value)
runLoopControl = raiseHandler $ fmap (either unLoopControl id) . runError
