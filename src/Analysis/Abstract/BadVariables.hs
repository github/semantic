{-# LANGUAGE GADTs, TypeOperators #-}
module Analysis.Abstract.BadVariables
( resumingBadVariables
) where

import Control.Abstract.Evaluator
import Data.Abstract.Evaluatable
import Prologue

resumingBadVariables :: (AbstractHole value, Effectful m, Show value) => m (Resumable (EvalError value) ': State [Name] ': effects) a -> m effects (a, [Name])
resumingBadVariables
  = runState []
  . raiseHandler (relay pure (\ (Resumable err) yield -> traceM ("EvalError" <> show err) *> case err of
    EnvironmentLookupError{} -> yield hole
    DefaultExportError{}     -> yield ()
    ExportError{}            -> yield ()
    IntegerFormatError{}     -> yield 0
    FloatFormatError{}       -> yield 0
    RationalFormatError{}    -> yield 0
    FreeVariableError name   -> modify' (name :) *> yield hole
    FreeVariablesError names -> modify' (names <>) *> yield (fromMaybeLast "unknown" names)))
