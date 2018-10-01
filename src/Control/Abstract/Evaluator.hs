{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators #-}
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
import Control.Monad.Effect.Exception as X
import qualified Control.Monad.Effect.Internal as Eff
import Control.Monad.Effect.NonDet    as X
import Control.Monad.Effect.Reader    as X
import Control.Monad.Effect.Resumable as X
import Control.Monad.Effect.State     as X
import Control.Monad.Effect.Trace     as X
import Control.Monad.IO.Class
import Prologue hiding (MonadError(..))

-- | An 'Evaluator' is a thin wrapper around 'Eff' with (phantom) type parameters for the address, term, and value types.
--
--   These parameters enable us to constrain the types of effects using them s.t. we can avoid both ambiguous types when they aren’t mentioned outside of the context, and lengthy, redundant annotations on the use sites of functions employing these effects.
--
--   These effects will typically include the environment, heap, module table, etc. effects necessary for evaluation of modules and terms, but may also include any other effects so long as they’re eventually handled.
newtype Evaluator address value effects a = Evaluator { runEvaluator :: Eff effects a }
  deriving (Applicative, Effectful, Functor, Monad)

deriving instance Member NonDet effects => Alternative (Evaluator address value effects)
deriving instance Member (Lift IO) effects => MonadIO (Evaluator address value effects)

-- Effects

-- | An effect for explicitly returning out of a function/method body.
newtype Return value = Return { unReturn :: value }
  deriving (Eq, Ord, Show)

earlyReturn :: Member (Exc (Return value)) effects
            => value
            -> Evaluator address value effects value
earlyReturn = throwError . Return

catchReturn :: (Member (Exc (Return value)) effects, Effectful (m address value)) => m address value effects value -> m address value effects value
catchReturn = Eff.raiseHandler (handleError (\ (Return addr) -> pure addr))

runReturn :: (Effectful (m address value), Effects effects) => m address value (Exc (Return value) ': effects) value -> m address value effects value
runReturn = Eff.raiseHandler (fmap (either unReturn id) . runError)


-- | Effects for control flow around loops (breaking and continuing).
data LoopControl value
  = Break    { unLoopControl :: value }
  | Continue { unLoopControl :: value }
  deriving (Eq, Ord, Show)

throwBreak :: Member (Exc (LoopControl value)) effects
           => value
           -> Evaluator address value effects value
throwBreak = throwError . Break

throwContinue :: Member (Exc (LoopControl value)) effects
              => value
              -> Evaluator address value effects value
throwContinue = throwError . Continue

catchLoopControl :: (Member (Exc (LoopControl value)) effects, Effectful (m address value)) => m address value effects a -> (LoopControl value -> m address value effects a) -> m address value effects a
catchLoopControl = catchError

runLoopControl :: (Effectful (m address value), Effects effects) => m address value (Exc (LoopControl value) ': effects) value -> m address value effects value
runLoopControl = Eff.raiseHandler (fmap (either unLoopControl id) . runError)
