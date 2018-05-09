{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Evaluating
( EvaluatingState(..)
, evaluating
) where

import Control.Abstract
import Data.Abstract.Address
import Data.Semilattice.Lower

-- | An analysis evaluating @term@s to @value@s with a list of @effects@ using 'Evaluatable', and producing incremental results of type @a@.
data EvaluatingState location value = EvaluatingState
  { environment :: Environment location value
  , heap        :: Heap location value
  , modules     :: ModuleTable (Maybe (Environment location value, value))
  , exports     :: Exports location value
  }

deriving instance (Eq (Cell location value), Eq location, Eq value) => Eq (EvaluatingState location value)
deriving instance (Ord (Cell location value), Ord location, Ord value) => Ord (EvaluatingState location value)
deriving instance (Show (Cell location value), Show location, Show value) => Show (EvaluatingState location value)


evaluating :: Evaluator location value
                (  Fail
                ': Fresh
                ': Reader (Environment location value)
                ': State (Environment location value)
                ': State (Heap location value)
                ': State (ModuleTable (Maybe (Environment location value, value)))
                ': State (Exports location value)
                ': effects) result
           -> Evaluator location value effects (Either String result, EvaluatingState location value)
evaluating
  = fmap (\ ((((result, env), heap), modules), exports) -> (result, EvaluatingState env heap modules exports))
  . runState lowerBound -- State (Exports location value)
  . runState lowerBound -- State (ModuleTable (Maybe (Environment location value, value)))
  . runState lowerBound -- State (Heap location value)
  . runState lowerBound -- State (Environment location value)
  . runReader lowerBound -- Reader (Environment location value)
  . runFresh 0
  . raiseHandler runFail
