{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Analysis.Abstract.Evaluating
( EvaluatingState(..)
, evaluating
) where

import Control.Abstract
import Data.Abstract.Address
import Data.Semilattice.Lower

-- | An analysis evaluating @term@s to @value@s with a list of @effects@ using 'Evaluatable', and producing incremental results of type @a@.
data EvaluatingState location term value = EvaluatingState
  { environment :: Environment location value
  , heap        :: Heap location value
  , modules     :: ModuleTable (Environment location value, value)
  , exports     :: Exports location value
  , jumps       :: JumpTable term
  }

deriving instance (Eq (Cell location value), Eq location, Eq term, Eq value) => Eq (EvaluatingState location term value)
deriving instance (Ord (Cell location value), Ord location, Ord term, Ord value) => Ord (EvaluatingState location term value)
deriving instance (Show (Cell location value), Show location, Show term, Show value) => Show (EvaluatingState location term value)


evaluating :: Evaluator location term value
                (  Fail
                ': Fresh
                ': Reader (Environment location value)
                ': State (Environment location value)
                ': State (Heap location value)
                ': State (ModuleTable (Environment location value, value))
                ': State (Exports location value)
                ': State (JumpTable term)
                ': effects) result
           -> Evaluator location term value effects (Either String result, EvaluatingState location term value)
evaluating
  = fmap (\ (((((result, env), heap), modules), exports), jumps) -> (result, EvaluatingState env heap modules exports jumps))
  . runState lowerBound -- State (JumpTable term)
  . runState lowerBound -- State (Exports location value)
  . runState lowerBound -- State (ModuleTable (Environment location value, value))
  . runState lowerBound -- State (Heap location value)
  . runState lowerBound -- State (Environment location value)
  . runReader lowerBound -- Reader (Environment location value)
  . runFresh 0
  . raiseHandler runFail
