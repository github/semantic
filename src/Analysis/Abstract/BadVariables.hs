{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.BadVariables
( BadVariables
) where

import Control.Abstract.Analysis
import Data.Abstract.Evaluatable
import Prologue

-- An analysis that resumes from evaluation errors and records the list of unresolved free variables.
newtype BadVariables m (effects :: [* -> *]) a = BadVariables (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (BadVariables m)

instance ( Effectful m
         , Member (Resumable (EvalError value)) effects
         , Member (State [Name]) effects
         , MonadAnalysis location term value effects m
         , MonadValue location value effects (BadVariables m)
         )
      => MonadAnalysis location term value effects (BadVariables m) where
  type Effects location term value (BadVariables m) = Resumable (EvalError value) ': State [Name] ': Effects location term value m

  analyzeTerm eval term = resume @(EvalError value) (liftAnalyze analyzeTerm eval term) (
        \yield err -> do
          traceM ("EvalError" <> show err)
          case err of
            DefaultExportError{}     -> yield ()
            ExportError{}            -> yield ()
            IntegerFormatError{}     -> yield 0
            FloatFormatError{}       -> yield 0
            RationalFormatError{}    -> yield 0
            FreeVariableError name   -> raise (modify' (name :)) >> hole >>= yield
            FreeVariablesError names -> raise (modify' (names <>)) >> yield (fromMaybeLast "unknown" names))

  analyzeModule = liftAnalyze analyzeModule

instance ( Interpreter effects (result, [Name]) rest m
         , MonadValue location value (State [Name] ': effects) m
         )
      => Interpreter (Resumable (EvalError value) ': State [Name] ': effects) result rest (BadVariables m) where
  interpret
    = interpret
    . raise @m
    . flip runState []
    . relay pure (\ (Resumable err) yield -> case err of
      DefaultExportError{}     -> yield ()
      ExportError{}            -> yield ()
      IntegerFormatError{}     -> yield 0
      FloatFormatError{}       -> yield 0
      RationalFormatError{}    -> yield 0
      FreeVariableError name   -> modify' (name :) >> lower @m hole >>= yield
      FreeVariablesError names -> modify' (names <>) >> yield (fromMaybeLast "unknown" names))
    . lower
