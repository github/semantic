{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For the Interpreter instance’s Evaluator constraint
module Analysis.Abstract.BadVariables
( BadVariables
) where

import Control.Abstract.Analysis
import Data.Abstract.Evaluatable
import Prologue

-- An analysis that resumes from evaluation errors and records the list of unresolved free variables.
newtype BadVariables m (effects :: [* -> *]) a = BadVariables { runBadVariables :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (BadVariables m)
deriving instance MonadAnalysis location term value effects m => MonadAnalysis location term value effects (BadVariables m)
deriving instance Evaluator location term value m => Evaluator location term value (BadVariables m)

instance ( AbstractHole value
         , Evaluator location term value m
         , Interpreter m effects
         , Show value
         )
      => Interpreter (BadVariables m) (Resumable (EvalError value) ': State [Name] ': effects) where
  type Result (BadVariables m) (Resumable (EvalError value) ': State [Name] ': effects) result = Result m effects (result, [Name])
  interpret
    = interpret
    . runBadVariables
    . raiseHandler
      ( flip runState []
      . relay pure (\ (Resumable err) yield -> traceM ("EvalError" <> show err) *> case err of
        EnvironmentLookupError{} -> yield hole
        DefaultExportError{}     -> yield ()
        ExportError{}            -> yield ()
        IntegerFormatError{}     -> yield 0
        FloatFormatError{}       -> yield 0
        RationalFormatError{}    -> yield 0
        FreeVariableError name   -> modify' (name :) *> yield hole
        FreeVariablesError names -> modify' (names <>) *> yield (fromMaybeLast "unknown" names)))
