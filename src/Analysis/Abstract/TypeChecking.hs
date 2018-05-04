{-# LANGUAGE TypeOperators #-}
module Analysis.Abstract.TypeChecking
( typeChecking
) where

import Control.Abstract.Evaluator
import Data.Abstract.Type

typeChecking :: Effectful m => m (Resumable TypeError ': effects) a -> m effects (Either (SomeExc TypeError) a)
typeChecking = raiseHandler runError
