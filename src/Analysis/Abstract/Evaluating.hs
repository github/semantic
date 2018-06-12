{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Evaluating
( EvaluatingState(..)
, evaluating
) where

import Control.Abstract
import Data.Semilattice.Lower

-- | An analysis evaluating @term@s to @value@s with a list of @effects@ using 'Evaluatable', and producing incremental results of type @a@.
data EvaluatingState address value = EvaluatingState
  { heap        :: Heap address (Cell address) value
  , modules     :: ModuleTable (Maybe (Environment address, address))
  }

deriving instance (Eq   (Cell address value), Eq   address, Eq   value) => Eq   (EvaluatingState address value)
deriving instance (Ord  (Cell address value), Ord  address, Ord  value) => Ord  (EvaluatingState address value)
deriving instance (Show (Cell address value), Show address, Show value) => Show (EvaluatingState address value)


evaluating :: Effects effects
           => Evaluator address value
                (  Fresh
                ': State (Heap address (Cell address) value)
                ': State (ModuleTable (Maybe (Environment address, address)))
                ': effects) result
           -> Evaluator address value effects (result, EvaluatingState address value)
evaluating
  = fmap (\ (modules, (heap, result)) -> (result, EvaluatingState heap modules))
  . runState lowerBound -- State (ModuleTable (Maybe (Environment address, address)))
  . runState lowerBound -- State (Heap address (Cell address) value)
  . runFresh 0
