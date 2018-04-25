{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.BadValues where

import Control.Abstract.Analysis
import Data.Abstract.Environment as Env
import Data.ByteString.Char8 (pack)
import Prologue

newtype BadValues m (effects :: [* -> *]) a = BadValues { runBadValues :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (BadValues m)

instance ( Effectful m
         , Member (Resumable (ValueError location value)) effects
         , MonadAnalysis location term value effects m
         , AbstractHole value
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
            CallError val     -> yield val
            StringError val   -> yield (pack (show val))
            BoolError{}       -> yield True
            NumericError{}    -> yield hole
            Numeric2Error{}   -> yield hole
            ComparisonError{} -> yield hole
            NamespaceError{}  -> getEnv >>= yield
            BitwiseError{}    -> yield hole
            Bitwise2Error{}   -> yield hole
            KeyValueError{}   -> yield (hole, hole)
            ArithmeticError{} -> yield hole
          )

  analyzeModule = liftAnalyze analyzeModule

instance ( Interpreter effects result rest m
         , MonadEvaluator location term value effects m
         , AbstractHole value
         , Show value
         )
      => Interpreter (Resumable (ValueError location value) ': effects) result rest (BadValues m) where
  interpret
    = interpret
    . runBadValues
    . raiseHandler (relay pure (\ (Resumable err) yield -> case err of
      ScopedEnvironmentError{} -> do
        env <- lower @m getEnv
        yield (Env.push env)
      CallError val     -> yield val
      StringError val   -> yield (pack (show val))
      BoolError{}       -> yield True
      NumericError{}    -> yield hole
      Numeric2Error{}   -> yield hole
      ComparisonError{} -> yield hole
      NamespaceError{}  -> lower @m getEnv >>= yield
      BitwiseError{}    -> yield hole
      Bitwise2Error{}   -> yield hole
      KeyValueError{}   -> yield (hole, hole)
      ArithmeticError{} -> yield hole))
