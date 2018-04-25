{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.BadValues where

import Control.Abstract.Analysis
import Data.Abstract.Environment as Env
import Data.ByteString.Char8 (pack)
import Prologue

newtype BadValues m (effects :: [* -> *]) a = BadValues (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m   => MonadEvaluator location term value effects (BadValues m)

instance ( Effectful m
         , Member (Resumable (ValueError location value)) effects
         , MonadAnalysis location term value effects m
         , MonadHole value effects (BadValues m)
         , Show value
         )
      => MonadAnalysis location term value effects (BadValues m) where
  analyzeTerm eval term = resume @(ValueError location value) (liftAnalyze analyzeTerm eval term) (
        \yield error -> do
          traceM ("ValueError" <> show error)
          case error of
            ScopedEnvironmentError{} -> do
              env <- getEnv
              yield (Env.push env)
            CallError val              -> yield val
            StringError val            -> yield (pack (show val))
            BoolError{}                -> yield True
            NumericError{}             -> hole >>= yield
            Numeric2Error{}            -> hole >>= yield
            ComparisonError{}          -> hole >>= yield
            NamespaceError{}           -> getEnv >>= yield
            BitwiseError{}             -> hole >>= yield
            Bitwise2Error{}            -> hole >>= yield
            KeyValueError{}            -> hole >>= \x -> yield (x, x)
          )

  analyzeModule = liftAnalyze analyzeModule

instance ( Interpreter effects result rest m
         , MonadEvaluator location term value effects m
         , MonadHole value effects m
         , Show value
         )
      => Interpreter (Resumable (ValueError location value) ': effects) result rest (BadValues m) where
  interpret = interpret . raise @m . relay pure (\ (Resumable err) yield -> case err of
    ScopedEnvironmentError{} -> do
      env <- lower @m getEnv
      yield (Env.push env)
    CallError val              -> yield val
    StringError val            -> yield (pack (show val))
    BoolError{}                -> yield True
    NumericError{}             -> lower @m hole >>= yield
    Numeric2Error{}            -> lower @m hole >>= yield
    ComparisonError{}          -> lower @m hole >>= yield
    NamespaceError{}           -> lower @m getEnv >>= yield
    BitwiseError{}             -> lower @m hole >>= yield
    Bitwise2Error{}            -> lower @m hole >>= yield
    KeyValueError{}            -> lower @m hole >>= \x -> yield (x, x)) . lower
