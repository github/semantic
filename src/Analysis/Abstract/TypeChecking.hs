{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.TypeChecking
( TypeChecking
) where

import Control.Abstract.Analysis
import Data.Abstract.Type
import Prologue hiding (TypeError)

newtype TypeChecking m (effects :: [* -> *]) a = TypeChecking { runTypeChecking :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance Evaluator location term value m => Evaluator location term value (TypeChecking m)
deriving instance AnalyzeModule location term value inner outer m => AnalyzeModule location term value inner outer (TypeChecking m)
deriving instance AnalyzeTerm location term value inner outer m => AnalyzeTerm location term value inner outer (TypeChecking m)

instance Interpreter m effects
      => Interpreter (TypeChecking m) (Resumable TypeError ': effects) where
  type Result (TypeChecking m) (Resumable TypeError ': effects) result = Result m effects (Either (SomeExc TypeError) result)
  interpret
    = interpret
    . runTypeChecking
    -- TODO: We should handle TypeError by yielding both sides of the exception,
    -- but something is mysteriously busted in the innards of typechecking,
    -- so doing that just yields an empty list in the result type, which isn't
    -- extraordinarily helpful. Better for now to just die with an error and
    -- tackle this issue in a separate PR.
    . raiseHandler runError
