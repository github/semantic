{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.CallGraph ( CallGraphing ) where

import           Analysis.Abstract.Graph
import           Control.Abstract.Analysis
import           Data.Abstract.Evaluatable (LoadError (..))
import           Data.Abstract.FreeVariables
import           Data.Abstract.Located
import           Data.Abstract.Module hiding (Module)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Syntax as Syntax
import           Data.Term
import           Prologue

newtype CallGraphing m (effects :: [* -> *]) a = CallGraphing { runCallGraphing :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (CallGraphing m)


instance ( Effectful m
         , Member (Resumable (LoadError term)) effects
         , Member (State Graph) effects
         , Member Syntax.Identifier syntax
         , MonadAnalysis (Located location term) term value effects m
         , term ~ Term (Union syntax) ann
         )
      => MonadAnalysis (Located location term) term value effects (CallGraphing m) where
  analyzeTerm eval term@(In _ syntax) = do
    case prj syntax of
      Just (Syntax.Identifier name) -> do
        moduleInclusion (Variable (unName name))
        variableDefinition name
      _ -> pure ()
    resume
      @(LoadError term)
      (liftAnalyze analyzeTerm eval term)
      (\yield (LoadError name) -> moduleInclusion (Module (BC.pack name)) >> yield [])

  analyzeModule recur m = do
    let name = BC.pack (modulePath (moduleInfo m))
    packageInclusion (Module name)
    moduleInclusion (Module name)
    liftAnalyze analyzeModule recur m

instance Interpreter m effects
      => Interpreter (CallGraphing m) (State Graph ': effects) where
  type Result (CallGraphing m) (State Graph ': effects) result = Result m effects (result, Graph)
  interpret = interpret . runCallGraphing . raiseHandler (`runState` mempty)
