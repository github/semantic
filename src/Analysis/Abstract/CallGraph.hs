{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving #-}
module Analysis.Abstract.CallGraph where

import Algebra.Graph
import Control.Abstract.Evaluator
import Control.Monad.Effect.Fail
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Evaluatable
import Data.Abstract.Linker
import Data.Abstract.Value

type CallGraphEffects term
  = '[ Fail
     , NonDetEff
     , State  (StoreFor CallGraph)
     , State  (EnvironmentFor CallGraph)
     , Reader (EnvironmentFor CallGraph)
     , Reader (Linker term)
     , State  (Linker CallGraph)
     ]

type CallGraph = Graph Name

newtype CallGraphAnalysis term a = CallGraphAnalysis { runCallGraphAnalysis :: Evaluator (CallGraphEffects term) term CallGraph a }
  deriving (Applicative, Functor, Monad, MonadFail)

deriving instance MonadEvaluator term CallGraph (CallGraphAnalysis term)
