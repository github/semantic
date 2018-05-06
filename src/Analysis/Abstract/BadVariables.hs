{-# LANGUAGE GADTs, TypeOperators #-}
module Analysis.Abstract.BadVariables
( resumingBadVariables
) where

import Control.Abstract.Evaluator
import Data.Abstract.Evaluatable
import Prologue

resumingBadVariables :: (AbstractHole value, Show value) => Evaluator location term value (Resumable (EvalError value) ': State [Name] ': effects) a -> Evaluator location term value effects (a, [Name])
resumingBadVariables
  = runState []
  . runEvalErrorWith (\ err -> traceM ("EvalError" <> show err) *> case err of
    EnvironmentLookupError{} -> pure hole
    DefaultExportError{}     -> pure ()
    ExportError{}            -> pure ()
    IntegerFormatError{}     -> pure 0
    FloatFormatError{}       -> pure 0
    RationalFormatError{}    -> pure 0
    FreeVariableError name   -> raise (modify' (name :)) *> pure hole
    FreeVariablesError names -> raise (modify' (names <>)) *> pure (fromMaybeLast "unknown" names))
