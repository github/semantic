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
  , Goto(..)
  , label
  , goto
  , runGoto
  , module X
  ) where

import Control.Monad.Effect           as X
import Control.Monad.Effect.Exception as Exc
import Control.Monad.Effect.Fresh     as X
import Control.Monad.Effect.Internal hiding (Return)
import qualified Control.Monad.Effect.Internal as Eff
import Control.Monad.Effect.NonDet    as X
import Control.Monad.Effect.Reader    as X
import Control.Monad.Effect.Resumable as X
import Control.Monad.Effect.State     as X
import Control.Monad.Effect.Trace     as X
import qualified Data.IntMap as IntMap
import Data.Semilattice.Lower
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

catchReturn :: Member (Return address) effects => Evaluator address value effects a -> (forall x . Return address (Eff effects) x -> Evaluator address value effects a) -> Evaluator address value effects a
catchReturn action handler = interpose pure (\ ret _ -> handler ret) action

runReturn :: (Effectful (m address value), Effects effects) => m address value (Return address ': effects) address -> m address value effects address
runReturn = raiseHandler (fmap (either id id) . Exc.runError . reinterpret (\ (Return value) -> Exc.throwError value))


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
runLoopControl = raiseHandler (fmap (either id id) . Exc.runError . reinterpret (\ eff -> case eff of
  Break    value -> Exc.throwError value
  Continue value -> Exc.throwError value))


type Label = Int

data Goto address m result where
  Label :: m address -> Goto address m Label
  Goto  :: Label     -> Goto address m address

instance Effect (Goto address) where
  handleState c dist (Request (Label action) k) = Request (Label (dist (action <$ c))) (dist . fmap k)
  handleState c dist (Request (Goto label) k) = Request (Goto label) (dist . (<$ c) . k)

label :: Member (Goto address) effects => Evaluator address value effects address -> Evaluator address value effects Label
label (Evaluator action) = send (Label action)

goto :: Member (Goto address) effects => Label -> Evaluator address value effects address
goto label = send (Goto label)

runGoto :: forall address value effects a. (Member Fresh effects, Effects effects) => Evaluator address value (Goto address ': effects) a -> Evaluator address value effects a
runGoto = raiseHandler (fmap snd . go lowerBound)
  where go :: forall a . IntMap (Eff (Goto address ': effects) address) -> Eff (Goto address ': effects) a -> Eff effects (IntMap (Eff (Goto address ': effects) address), a)
        go table (Eff.Return a)            = pure (table, a)
        go table (Effect (Label action) k) = do
          i <- fresh
          go (IntMap.insert i action table) (k i)
        go table (Effect (Goto label) k)   = go table (maybe (error ("programmer error: attempted to goto the unallocated label " <> show label)) (>>= k) (IntMap.lookup label table))
        go table (Other u k)               = liftStatefulHandler (table, ()) (uncurry go) u k
