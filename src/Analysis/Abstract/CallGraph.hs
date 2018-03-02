{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving #-}
module Analysis.Abstract.CallGraph where

import Control.Abstract.Evaluator
import Control.Monad.Effect.Fail
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Linker
import Data.Abstract.Value

type CallGraphEffects term value
  = '[ Fail
     , NonDetEff
     , State  (StoreFor value)
     , State  (EnvironmentFor value)
     , Reader (EnvironmentFor value)
     , Reader (Linker term)
     , State  (Linker value)
     ]

newtype CallGraphAnalysis term value a = CallGraphAnalysis { runCallGraphAnalysis :: Evaluator (CallGraphEffects term value) term value a }
  deriving (Applicative, Functor, Monad)
