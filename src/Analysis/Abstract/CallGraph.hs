{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, UndecidableInstances #-}
module Analysis.Abstract.CallGraph where

import Algebra.Graph.Class
import Analysis.CallGraph
import Control.Abstract.Evaluator
import Control.Effect
import Control.Monad.Effect.Fail
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.Evaluatable
import Data.Abstract.Linker
import Data.Abstract.Value
import Prologue hiding (empty)

type CallGraphEffects term
  = '[ Fail
     , NonDetEff
     , State  CallGraph
     , State  (StoreFor CallGraph)
     , State  (EnvironmentFor CallGraph)
     , Reader (EnvironmentFor CallGraph)
     , Reader (Linker term)
     , State  (Linker CallGraph)
     ]

getCallGraph :: CallGraphAnalysis term CallGraph
getCallGraph = CallGraphAnalysis (Evaluator get)

modifyCallGraph :: (CallGraph -> CallGraph) -> CallGraphAnalysis term ()
modifyCallGraph f = CallGraphAnalysis (Evaluator (modify f))


newtype CallGraphAnalysis term a = CallGraphAnalysis { runCallGraphAnalysis :: Evaluator (CallGraphEffects term) term CallGraph a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail)

deriving instance MonadEvaluator term CallGraph (CallGraphAnalysis term)

evaluateCallGraph :: forall term
                  .  ( Evaluatable (Base term)
                     , Foldable (Base term)
                     , FreeVariables term
                     , IsDeclaration (Base term)
                     , MonadAddressable (LocationFor CallGraph) CallGraph (CallGraphAnalysis term)
                     , MonadValue term CallGraph (CallGraphAnalysis term)
                     , Ord (LocationFor CallGraph)
                     , Ord term
                     , Recursive term
                     , Semigroup (Cell (LocationFor CallGraph) CallGraph)
                     )
                  => term
                  -> Final (CallGraphEffects term) CallGraph
evaluateCallGraph = run @(CallGraphEffects term) . runEvaluator . runCallGraphAnalysis . evaluateTerm

instance MonadValue term CallGraph (CallGraphAnalysis term) where
  unit = pure empty
  integer _ = pure empty
  boolean _ = pure empty
  string _ = pure empty

  ifthenelse _ then' else' = overlay <$> then' <*> else'

  abstract names body = foldr bindLocally (subtermValue body) names
    where bindLocally name rest = do
            addr <- alloc name
            assign addr empty
            localEnv (envInsert name addr) rest

  apply operator arguments = foldr overlay operator <$> traverse subtermValue arguments

type instance LocationFor CallGraph = Monovariant


instance ( Evaluatable (Base term)
         , FreeVariables term
         , IsDeclaration (Base term)
         , MonadAddressable (LocationFor CallGraph) CallGraph (CallGraphAnalysis term)
         , MonadValue term CallGraph (CallGraphAnalysis term)
         , Ord term
         , Recursive term
         , Semigroup (Cell (LocationFor CallGraph) CallGraph)
         )
         => MonadAnalysis term CallGraph (CallGraphAnalysis term) where
  evaluateTerm = foldSubterms eval
