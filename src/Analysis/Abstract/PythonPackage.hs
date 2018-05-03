{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.PythonPackage
( PythonPackaging, Strategy(..) ) where

import Analysis.Abstract.Graph
import           Control.Abstract.Analysis
import Control.Monad.Effect.Internal (interpose)
import           Data.Abstract.Evaluatable (EvalError, LoadError (..), variable)
import           Data.Abstract.FreeVariables (name)
import           Data.Abstract.Value (Value)
import           Control.Abstract.Value
import           Data.Abstract.Located
import           Data.Abstract.Module hiding (Module)
import qualified Data.ByteString.Char8 as BC
import           Data.Term
import           Prologue

newtype PythonPackaging m (effects :: [* -> *]) a = PythonPackaging { runPythonPackaging :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (PythonPackaging m)

data Strategy = Unknown | Packages [ByteString] | FindPackages
  deriving (Show)

instance ( Effectful m
         , Member (State Strategy) effects
         , Member (EvalClosure term value) effects
         , Member (Resumable (AddressError location value)) effects
         , Member (Resumable (EvalError value)) effects
         , MonadAnalysis location term value effects m
         , MonadValue location value effects m
         , MonadAddressable location effects m
         )
      => MonadAnalysis location term value effects (PythonPackaging m) where
  analyzeTerm eval term = raiseHandler (interpose @(EvalClosure term value) pure $ \(EvalClosure term) yield -> do
    traceM "In PythonPackaging"
    lower @m (do
      -- Guard on setup call
      value <- variable (name "packages")
      as <- asArray value
      as' <- traverse asString as
      raise (put (Packages as'))
      evaluateClosureBody term) >>= yield
    ) (liftAnalyze analyzeTerm eval term)

  analyzeModule recur m = liftAnalyze analyzeModule recur m

instance Interpreter m effects
      => Interpreter (PythonPackaging m) (State Strategy ': effects) where
  type Result (PythonPackaging m) (State Strategy ': effects) result = Result m effects (result, Strategy)
  interpret = interpret . runPythonPackaging . raiseHandler (`runState` Unknown)
