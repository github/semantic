{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.BadValues where

import Control.Abstract.Analysis
import Data.Abstract.Evaluatable
import Analysis.Abstract.Evaluating
import Data.Abstract.Environment as Env
import Prologue
import Data.ByteString.Char8 (pack)

newtype BadValues m (effects :: [* -> *]) a = BadValues (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh)

deriving instance MonadControl term (m effects)                    => MonadControl term (BadValues m effects)
deriving instance MonadEnvironment location value (m effects)      => MonadEnvironment location value (BadValues m effects)
deriving instance MonadHeap location value (m effects)             => MonadHeap location value (BadValues m effects)
deriving instance MonadModuleTable location term value (m effects) => MonadModuleTable location term value (BadValues m effects)
deriving instance MonadEvaluator location term value (m effects)   => MonadEvaluator location term value (BadValues m effects)

instance ( Effectful m
         , Member (Resumable (ValueError location value)) effects
         , Member (State (EvaluatingState location term value)) effects
         , Member (State [Name]) effects
         , MonadAnalysis location term value (m effects)
         , MonadValue location value (BadValues m effects)
         )
      => MonadAnalysis location term value (BadValues m effects) where
  type Effects location term value (BadValues m effects) = State [Name] ': Effects location term value (m effects)

  analyzeTerm eval term = resumeException @(ValueError location value) (liftAnalyze analyzeTerm eval term) (
        \yield error -> case error of
          ScopedEnvironmentError{} -> do
            env <- getEnv
            yield (Env.push env)
          CallError val              -> yield val
          StringError val            -> yield (pack $ show val)
          BoolError{}                -> yield True
          NumericError{}             -> unit >>= yield
          Numeric2Error{}            -> unit >>= yield
          ComparisonError{}          -> unit >>= yield
          NamespaceError{}           -> getEnv >>= yield
          BitwiseError{}             -> unit >>= yield
          Bitwise2Error{}            -> unit >>= yield
          KeyValueError{}            -> unit >>= \x -> yield (x, x)
          )

  analyzeModule = liftAnalyze analyzeModule
