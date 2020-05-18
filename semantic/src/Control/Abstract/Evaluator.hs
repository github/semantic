{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
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

import Control.Algebra
import Control.Carrier.Error.Either
import Control.Effect.Error as X
import Control.Effect.Fresh as X
import Control.Effect.NonDet as X
import Control.Effect.Reader as X
import Control.Effect.Resumable as X
import Control.Effect.State as X
import Control.Effect.Trace as X
import Control.Monad.IO.Class
import Data.Coerce

-- | An 'Evaluator' is a thin wrapper around a monad with (phantom) type parameters for the address, term, and value types.
--
--   These parameters enable us to constrain the types of effects using them s.t. we can avoid both ambiguous types when they aren’t mentioned outside of the context, and lengthy, redundant annotations on the use sites of functions employing these effects.
--
--   These effects will typically include the environment, heap, module table, etc. effects necessary for evaluation of modules and terms, but may also include any other effects so long as they’re eventually handled.
newtype Evaluator term address value m a = Evaluator { runEvaluator :: m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadIO)

instance Algebra sig m => Algebra sig (Evaluator term address value m) where
  alg = Evaluator . alg . handleCoercible

-- | Raise a handler on monads into a handler on 'Evaluator's over those monads.
raiseHandler :: (m a -> n b)
             -> Evaluator term address value m a
             -> Evaluator term address value n b
raiseHandler = coerce


-- | An open-recursive function.
type Open a = a -> a


-- Effects

-- | An effect for explicitly returning out of a function/method body.
newtype Return value = Return { unReturn :: value }
  deriving (Eq, Ord, Show)

earlyReturn :: Has (Throw (Return value)) sig m
            => value
            -> Evaluator term address value m value
earlyReturn = throwError . Return

catchReturn :: Has (Catch (Return value)) sig m
            => Evaluator term address value m value
            -> Evaluator term address value m value
catchReturn = flip catchError (\ (Return value) -> pure value)

runReturn :: Algebra sig m
          => Evaluator term address value (ErrorC (Return value) m) value
          -> Evaluator term address value m value
runReturn = raiseHandler $ fmap (either unReturn id) . runError


-- | Effects for control flow around loops (breaking and continuing).
data LoopControl value
  = Break    value
  | Continue value
  | Abort
  deriving (Eq, Ord, Show)

unLoopControl :: LoopControl value -> value
unLoopControl = \case
  Break    v -> v
  Continue v -> v
  Abort      -> error "unLoopControl: Abort"

throwBreak :: Has (Error (LoopControl value)) sig m
           => value
           -> Evaluator term address value m value
throwBreak = throwError . Break

throwContinue :: Has (Error (LoopControl value)) sig m
              => value
              -> Evaluator term address value m value
throwContinue = throwError . Continue

throwAbort :: forall term address sig m value a . Has (Error (LoopControl value)) sig m
           => Evaluator term address value m a
throwAbort = throwError (Abort @value)

catchLoopControl :: Has (Error (LoopControl value)) sig m
                 => Evaluator term address value m a
                 -> (LoopControl value -> Evaluator term address value m a)
                 -> Evaluator term address value m a
catchLoopControl = catchError

runLoopControl :: Algebra sig m
               => Evaluator term address value (ErrorC (LoopControl value) m) value
               -> Evaluator term address value m value
runLoopControl = raiseHandler $ fmap (either unLoopControl id) . runError
