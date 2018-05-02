{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, UndecidableInstances #-}
module Analysis.Abstract.Evaluating
( Evaluating
, EvaluatingState(..)
) where

import Control.Abstract.Analysis
import qualified Control.Monad.Effect as Eff
import Data.Abstract.Address
import Data.Abstract.Origin
import Data.Semilattice.Lower
import Prologue

-- | An analysis evaluating @term@s to @value@s with a list of @effects@ using 'Evaluatable', and producing incremental results of type @a@.
newtype Evaluating location term value effects a = Evaluating { runEvaluating :: Eff effects a }
  deriving (Applicative, Functor, Effectful, Evaluator location term value, Monad)

deriving instance Member NonDet effects => Alternative (Evaluating location term value effects)

data EvaluatingState location term value = EvaluatingState
  { environment :: Environment location value
  , heap        :: Heap location value
  , modules     :: ModuleTable (Environment location value, value)
  , exports     :: Exports location value
  , jumps       :: JumpTable term
  }

deriving instance (Eq (Cell location value), Eq location, Eq term, Eq value, Eq (Base term ())) => Eq (EvaluatingState location term value)
deriving instance (Ord (Cell location value), Ord location, Ord term, Ord value, Ord (Base term ())) => Ord (EvaluatingState location term value)
deriving instance (Show (Cell location value), Show location, Show term, Show value, Show (Base term ())) => Show (EvaluatingState location term value)


-- | Effects necessary for evaluating (whether concrete or abstract).
type EvaluatingEffects location term value
  = '[ EvalClosure term value
     , Return value
     , LoopControl value
     , Fail                                        -- Failure with an error message
     , Fresh                                       -- For allocating new addresses and/or type variables.
     , Reader (SomeOrigin term)                    -- The current term’s origin.
     , Reader (Environment location value)         -- Default environment used as a fallback in lookupEnv
     , Reader LoadStack
     , State (Environment location value)
     , State (Heap location value)
     , State (ModuleTable (Environment location value, value))
     , State (Exports location value)
     , State (JumpTable term)
     ]

instance ( Member (Reader (Environment location value)) effects
         , Member (Reader LoadStack) effects
         , Member (Reader (SomeOrigin term)) effects
         , Member (State (Environment location value)) effects
         , Member (State (Heap location value)) effects
         , Member (State (ModuleTable (Environment location value, value))) effects
         , Member (State (Exports location value)) effects
         , Member (State (JumpTable term)) effects
         )
      => MonadEvaluator location term value effects (Evaluating location term value)

instance ( Corecursive term
         , Member (Reader (Environment location value)) effects
         , Member (Reader LoadStack) effects
         , Member (Reader (SomeOrigin term)) effects
         , Member (State (Environment location value)) effects
         , Member (State (Heap location value)) effects
         , Member (State (ModuleTable (Environment location value, value))) effects
         , Member (State (Exports location value)) effects
         , Member (State (JumpTable term)) effects
         , Recursive term
         )
      => MonadAnalysis location term value effects (Evaluating location term value) where
  analyzeTerm eval term = pushOrigin (termOrigin (embedSubterm term)) (eval term)

  analyzeModule eval m = pushOrigin (moduleOrigin (subterm <$> m)) (eval m)


instance (AbstractHole value, Show term, Show value) => Interpreter (Evaluating location term value) (EvaluatingEffects location term value) where
  type Result (Evaluating location term value) (EvaluatingEffects location term value) result
    = ( Either String result
      , EvaluatingState location term value)
  interpret
    = (\ (((((result, env), heap), modules), exports), jumps) -> (result, EvaluatingState env heap modules exports jumps))
    . interpret
    . runEvaluating
    . raiseHandler
      ( flip runState  lowerBound -- State (JumpTable term)
      . flip runState  lowerBound -- State (Exports location value)
      . flip runState  lowerBound -- State (ModuleTable (Environment location value, value))
      . flip runState  lowerBound -- State (Heap location value)
      . flip runState  lowerBound -- State (Environment location value)
      . flip runReader lowerBound -- Reader LoadStack
      . flip runReader lowerBound -- Reader (Environment location value)
      . flip runReader lowerBound -- Reader (SomeOrigin term)
      . flip runFresh' 0
      . runFail
      -- NB: We should never have a 'Return', 'Break', or 'Continue' at this point in execution; the scope being returned from/broken from/continued should have intercepted the effect. This handler will therefore only be invoked if we issue a 'Return', 'Break', or 'Continue' outside of such a scope, and unfortunately if this happens it will handle it by resuming the scope being returned from. While it would be _slightly_ more correct to instead exit with the value being returned, we aren’t able to do that here since 'Interpreter'’s type is parametric in the value being returned—we don’t know that we’re returning a @value@ (because we very well may not be). On the balance, I felt the strange behaviour in error cases is worth the improved behaviour in the common case—we get to lose a layer of 'Either' in the result for each.
      -- In general, it’s expected that none of the following effects will remain by the time 'interpret' is called—they should have been handled by local 'interpose's—but if they do, we’ll at least trace.
      . Eff.interpret (\ control -> case control of
        Break value -> traceM ("Evaluating.interpret: resuming uncaught break with " <> show value) $> value
        Continue    -> traceM "Evaluating.interpret: resuming uncaught continue with hole" $> hole)
      . Eff.interpret (\ (Return value) -> traceM ("Evaluating.interpret: resuming uncaught return with " <> show value) $> value)
      . Eff.interpret (\ (EvalClosure term) -> traceM ("Evaluating.interpret: resuming uncaught EvalClosure of " <> show term <> " with hole") $> hole))
      -- TODO: Replace 'traceM's with e.g. 'Telemetry'.
