{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Analysis.Abstract.Evaluating
( EvaluatingState(..)
, EvaluatingEffects
, evaluating
) where

import Control.Abstract.Evaluator
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
  = '[ Fail                                -- Failure with an error message
     , Fresh                               -- For allocating new addresses and/or type variables.
     , Reader (Environment location value) -- Default environment used as a fallback in lookupEnv
     , State (Environment location value)
     , State (Heap location value)
     , State (ModuleTable (Environment location value, value))
     , State (Exports location value)
     , State (JumpTable term)
     ]


evaluating :: Evaluator location term value (EvaluatingEffects location term value) result -> (Either String result, EvaluatingState location term value)
evaluating
  = (\ (((((result, env), heap), modules), exports), jumps) -> (result, EvaluatingState env heap modules exports jumps))
  . Eff.run
  . lower
  . runState lowerBound -- State (JumpTable term)
  . runState lowerBound -- State (Exports location value)
  . runState lowerBound -- State (ModuleTable (Environment location value, value))
  . runState lowerBound -- State (Heap location value)
  . runState lowerBound -- State (Environment location value)
  . runReader lowerBound -- Reader (Environment location value)
  . raiseHandler
    ( flip runFresh' 0
    . runFail)
