{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Evaluating
( EvaluatingState(..)
, evaluating
) where

import Control.Abstract
import Data.Semilattice.Lower

-- | An analysis evaluating @term@s to @value@s with a list of @effects@ using 'Evaluatable', and producing incremental results of type @a@.
data EvaluatingState address value = EvaluatingState
  { environment :: Environment address
  , heap        :: Heap address (Cell address) value
  , modules     :: ModuleTable (Maybe (Environment address, value))
  , exports     :: Exports address
  }

deriving instance (Eq (Cell address value), Eq address, Eq value) => Eq (EvaluatingState address value)
deriving instance (Ord (Cell address value), Ord address, Ord value) => Ord (EvaluatingState address value)
deriving instance (Show (Cell address value), Show address, Show value) => Show (EvaluatingState address value)


evaluating :: Evaluator address value
                (  Fail
                ': Fresh
                ': State (Environment address)
                ': State (Heap address (Cell address) value)
                ': State (ModuleTable (Maybe (Environment address, value)))
                ': State (Exports address)
                ': effects) result
           -> Evaluator address value effects (Either String result, EvaluatingState address value)
evaluating
  = fmap (\ ((((result, env), heap), modules), exports) -> (result, EvaluatingState env heap modules exports))
  . runState lowerBound -- State (Exports address)
  . runState lowerBound -- State (ModuleTable (Maybe (Environment address, value)))
  . runState lowerBound -- State (Heap address (Cell address) value)
  . runState lowerBound -- Env address
  . runFresh 0
  . runFail
