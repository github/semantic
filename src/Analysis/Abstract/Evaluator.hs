{-# LANGUAGE DataKinds, FunctionalDependencies, RankNTypes, UndecidableInstances #-}
module Analysis.Abstract.Evaluator where

import Control.Applicative
import Control.Monad.Effect
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Linker
import Data.Abstract.Value
import Prelude hiding (fail)

class Monad m => MonadEvaluator term value m | m -> term, m -> value where
  getGlobalEnv :: m (EnvironmentFor value)
  modifyGlobalEnv :: (EnvironmentFor value -> EnvironmentFor value) -> m ()

  askLocalEnv :: m (EnvironmentFor value)
  localEnv :: (EnvironmentFor value -> EnvironmentFor value) -> m a -> m a

  getStore :: m (StoreFor value)
  modifyStore :: (StoreFor value -> StoreFor value) -> m ()

  getModuleTable :: m (Linker value)
  modifyModuleTable :: (Linker value -> Linker value) -> m ()

  askModuleTable :: m (Linker term)
  localModuleTable :: (Linker term -> Linker term) -> m a -> m a

instance MonadEvaluator term value (Evaluator effects term value) where
  getGlobalEnv = Evaluator get
  modifyGlobalEnv f = Evaluator (modify f)

  askLocalEnv = Evaluator ask
  localEnv f a = Evaluator (local f (runEvaluator a))

  getStore = Evaluator get
  modifyStore f = Evaluator (modify f)

  getModuleTable = Evaluator get
  modifyModuleTable f = Evaluator (modify f)

  askModuleTable = Evaluator ask
  localModuleTable f a = Evaluator (local f (runEvaluator a))


newtype Evaluator effects term value a
  = Evaluator
    { runEvaluator :: Members '[ Fail
                               , Reader (EnvironmentFor value)
                               , State  (EnvironmentFor value)
                               , State  (StoreFor value)
                               , Reader (Linker term)
                               , State  (Linker value)
                               ] effects
                   => Eff effects a
    }


instance Functor (Evaluator effects term value) where
  fmap f (Evaluator run) = Evaluator (fmap f run)

instance Applicative (Evaluator effects term value) where
  pure = Evaluator . pure

  Evaluator runF <*> Evaluator runA = Evaluator (runF <*> runA)

instance Member NonDetEff effects => Alternative (Evaluator effects term value) where
  empty = Evaluator empty

  Evaluator runA <|> Evaluator runB = Evaluator (runA <|> runB)

instance Monad (Evaluator effects term value) where
  return = pure

  Evaluator runA >>= f = Evaluator (runA >>= runEvaluator . f)

instance MonadFail (Evaluator effects term value) where
  fail s = Evaluator (fail s)

instance Member Fresh effects => MonadFresh (Evaluator effects term value) where
  fresh = Evaluator fresh

  reset t = Evaluator (reset t)
