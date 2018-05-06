{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Analysis.Abstract.Evaluating
( EvaluatingState(..)
, EvaluatingEffects
, evaluating
) where

import Control.Abstract.Evaluator
import Control.Abstract.Value
import qualified Control.Monad.Effect.Internal as Eff
import Data.Abstract.Address
import Data.Semilattice.Lower
import Prologue

-- | An analysis evaluating @term@s to @value@s with a list of @effects@ using 'Evaluatable', and producing incremental results of type @a@.
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
  = '[ LoopControl value
     , Fail                                -- Failure with an error message
     , Fresh                               -- For allocating new addresses and/or type variables.
     , Reader (Environment location value) -- Default environment used as a fallback in lookupEnv
     , State (Environment location value)
     , State (Heap location value)
     , State (ModuleTable (Environment location value, value))
     , State (Exports location value)
     , State (JumpTable term)
     ]


evaluating :: (AbstractHole value, Show value) => Evaluator location term value (EvaluatingEffects location term value) result -> (Either String result, EvaluatingState location term value)
evaluating
  = (\ (((((result, env), heap), modules), exports), jumps) -> (result, EvaluatingState env heap modules exports jumps))
  . Eff.run
  . lower
  . handleState lowerBound -- State (JumpTable term)
  . handleState lowerBound -- State (Exports location value)
  . handleState lowerBound -- State (ModuleTable (Environment location value, value))
  . handleState lowerBound -- State (Heap location value)
  . handleState lowerBound -- State (Environment location value)
  . handleReader lowerBound -- Reader (Environment location value)
  . raiseHandler
    ( flip runFresh' 0
    . runFail
    -- NB: We should never have a 'Return', 'Break', or 'Continue' at this point in execution; the scope being returned from/broken from/continued should have intercepted the effect. This handler will therefore only be invoked if we issue a 'Return', 'Break', or 'Continue' outside of such a scope, and unfortunately if this happens it will handle it by resuming the scope being returned from. While it would be _slightly_ more correct to instead exit with the value being returned, we aren’t able to do that here since 'Interpreter'’s type is parametric in the value being returned—we don’t know that we’re returning a @value@ (because we very well may not be). On the balance, I felt the strange behaviour in error cases is worth the improved behaviour in the common case—we get to lose a layer of 'Either' in the result for each.
    -- In general, it’s expected that none of the following effects will remain by the time 'interpret' is called—they should have been handled by local 'interpose's—but if they do, we’ll at least trace.
    . Eff.interpret (\ control -> case control of
      Break value -> traceM ("Evaluating.interpret: resuming uncaught break with " <> show value) $> value
      Continue    -> traceM "Evaluating.interpret: resuming uncaught continue with hole" $> hole))
    -- TODO: Replace 'traceM's with e.g. 'Telemetry'.
