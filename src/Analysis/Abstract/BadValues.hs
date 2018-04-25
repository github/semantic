{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.BadValues where

import Control.Abstract.Analysis
import Data.Abstract.Evaluatable
import Data.Abstract.Environment as Env
import Prologue
import Data.ByteString.Char8 (pack)

newtype BadValues m (effects :: [* -> *]) a = BadValues (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m   => MonadEvaluator location term value effects (BadValues m)

instance ( Effectful m
         , Member (Resumable (ValueError location value)) effects
         , Member (State [Name]) effects
         , MonadAnalysis location term value effects m
         , MonadValue location value effects (BadValues m)
         )
      => MonadAnalysis location term value effects (BadValues m) where
  type Effects location term value (BadValues m) = State [Name] ': Effects location term value m

  analyzeTerm eval term = resume @(ValueError location value) (liftAnalyze analyzeTerm eval term) (
        \yield error -> do
          traceM ("ValueError" <> show error)
          case error of
            ScopedEnvironmentError{} -> do
              env <- getEnv
              yield (Env.push env)
            CallError val              -> yield val
            StringError val            -> yield (pack $ show val)
            BoolError{}                -> yield True
            NumericError{}             -> hole >>= yield
            Numeric2Error{}            -> hole >>= yield
            ComparisonError{}          -> hole >>= yield
            NamespaceError{}           -> getEnv >>= yield
            BitwiseError{}             -> hole >>= yield
            Bitwise2Error{}            -> hole >>= yield
            KeyValueError{}            -> hole >>= \x -> yield (x, x)
            ArithmeticError{}          -> hole >>= yield
          )

  analyzeModule = liftAnalyze analyzeModule
