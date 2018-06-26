{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, LambdaCase, KindSignatures, RankNTypes, TypeOperators #-}
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
import Prologue hiding (MonadError(..))

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
newtype Return address = Return { unReturn :: address }
  deriving (Eq, Ord, Show)

earlyReturn :: Member (Exc (Return address)) effects
            => address
            -> Evaluator address value effects address
earlyReturn = throwError . Return

catchReturn :: (Member (Exc (Return address)) effects, Effectful (m address value)) => m address value effects address -> m address value effects address
catchReturn = Eff.raiseHandler (handleError (\ (Return addr) -> pure addr))

runReturn :: (Effectful (m address value), Effects effects) => m address value (Exc (Return address) ': effects) address -> m address value effects address
runReturn = Eff.raiseHandler (fmap (either unReturn id) . runError)


-- | Effects for control flow around loops (breaking and continuing).
data LoopControl address (m :: * -> *) resume where
  Break    :: address -> LoopControl address m address
  Continue :: address -> LoopControl address m address

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

catchLoopControl :: (Member (LoopControl address) effects, Effectful (m address value)) => m address value effects a -> (forall x . LoopControl address (Eff effects) x -> m address value effects a) -> m address value effects a
catchLoopControl action handler = Eff.raiseHandler (interpose (\ control _ -> Eff.lowerEff (handler control))) action

runLoopControl :: (Effectful (m address value), Effects effects) => m address value (LoopControl address ': effects) address -> m address value effects address
runLoopControl = Eff.raiseHandler go . (`catchLoopControl` (\ control -> case control of
  Break    address -> Eff.raiseEff (pure address)
  Continue address -> Eff.raiseEff (pure address)))
  where go :: Effects effects => Eff (LoopControl address ': effects) a -> Eff effects a
        go (Eff.Return a) = pure a
        go (Effect (Break a) k) = go (k a)
        go (Effect (Continue a) k) = go (k a)
        go (Other u k) = liftHandler go u k
