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
import Prologue hiding (empty)

type CallGraphEffects term
  = '[ Fail
     , NonDetEff
     , State  (StoreFor CallGraphS)
     , State  (EnvironmentFor CallGraphS)
     , Reader (EnvironmentFor CallGraphS)
     , Reader (Linker term)
     , State  (Linker CallGraphS)
     ]

type CallGraph = Graph Name
type CallGraphS = CallGraph -> CallGraph

newtype CallGraphAnalysis term a = CallGraphAnalysis { runCallGraphAnalysis :: Evaluator (CallGraphEffects term) term CallGraphS a }
  deriving (Applicative, Functor, Monad, MonadFail)

instance MonadEvaluator term CallGraphS (CallGraphAnalysis term) where
  getGlobalEnv = CallGraphAnalysis getGlobalEnv
  modifyGlobalEnv f = CallGraphAnalysis (modifyGlobalEnv f)

  askLocalEnv = CallGraphAnalysis askLocalEnv
  localEnv f a = CallGraphAnalysis (localEnv f (runCallGraphAnalysis a))

  lookupWith with name = do
    addr <- lookupLocalEnv name
    maybe (pure Nothing) connectWith addr
    where connectWith addr = do
            v <- with addr
            pure (Just (connect <*> v))

  getStore = CallGraphAnalysis getStore
  modifyStore f = CallGraphAnalysis (modifyStore f)

  getModuleTable = CallGraphAnalysis getModuleTable
  modifyModuleTable f = CallGraphAnalysis (modifyModuleTable f)

  askModuleTable = CallGraphAnalysis askModuleTable
  localModuleTable f a = CallGraphAnalysis (localModuleTable f (runCallGraphAnalysis a))


instance MonadValue term CallGraphS (CallGraphAnalysis term) where
  unit = pure id
  integer _ = pure id
  boolean _ = pure id
  string _ = pure id

  ifthenelse _ then' else' = liftA2 overlay <$> then' <*> else'

  abstract _ = subtermValue

  apply operator arguments = foldr (liftA2 overlay) operator <$> traverse subtermValue arguments
