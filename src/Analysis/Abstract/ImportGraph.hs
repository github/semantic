{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.ImportGraph
( ImportGraphing ) where

import Analysis.Abstract.Graph
import           Control.Abstract.Analysis
import           Data.Abstract.Evaluatable (LoadError (..))
import           Data.Abstract.Located
import           Data.Abstract.Module hiding (Module)
import qualified Data.ByteString.Char8 as BC
import           Data.Term
import           Prologue

newtype ImportGraphing m (effects :: [* -> *]) a = ImportGraphing { runImportGraphing :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (ImportGraphing m)


instance ( Effectful m
         , Member (Resumable (LoadError term)) effects
         , Member (State Graph) effects
         , MonadAnalysis (Located location term) term value effects m
         , term ~ Term (Union syntax) ann
         )
      => MonadAnalysis (Located location term) term value effects (ImportGraphing m) where
  analyzeTerm eval term =
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
      => Interpreter (ImportGraphing m) (State Graph ': effects) where
  type Result (ImportGraphing m) (State Graph ': effects) result = Result m effects (result, Graph)
  interpret = interpret . runImportGraphing . raiseHandler (`runState` mempty)
