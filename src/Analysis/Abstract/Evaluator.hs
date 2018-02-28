{-# LANGUAGE DataKinds, RankNTypes, UndecidableInstances #-}
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

getGlobalEnv :: Evaluator effects term value (EnvironmentFor value)
getGlobalEnv = Evaluator get

modifyGlobalEnv :: (EnvironmentFor value -> EnvironmentFor value) -> Evaluator effects term value ()
modifyGlobalEnv f = Evaluator (modify f)


askLocalEnv :: Evaluator effects term value (EnvironmentFor value)
askLocalEnv = Evaluator ask

localEnv :: (EnvironmentFor value -> EnvironmentFor value) -> Evaluator effects term value a -> Evaluator effects term value a
localEnv f a = Evaluator (local f (runEvaluator a))


getStore :: Evaluator effects term value (StoreFor value)
getStore = Evaluator get

modifyStore :: (StoreFor value -> StoreFor value) -> Evaluator effects term value ()
modifyStore f = Evaluator (modify f)


getModuleTable :: Evaluator effects term value (Linker value)
getModuleTable = Evaluator get

modifyModuleTable :: (Linker value -> Linker value) -> Evaluator effects term value ()
modifyModuleTable f = Evaluator (modify f)


askModuleTable :: Evaluator effects term value (Linker term)
askModuleTable = Evaluator ask

localModuleTable :: (Linker term -> Linker term) -> Evaluator effects term value a -> Evaluator effects term value a
localModuleTable f a = Evaluator (local f (runEvaluator a))


data Evaluator effects term value a
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
