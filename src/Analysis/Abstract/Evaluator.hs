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

getGlobalEnv :: Evaluator effects value (EnvironmentFor value)
getGlobalEnv = Evaluator get

modifyGlobalEnv :: (EnvironmentFor value -> EnvironmentFor value) -> Evaluator effects value ()
modifyGlobalEnv f = Evaluator (modify f)


askLocalEnv :: Evaluator effects value (EnvironmentFor value)
askLocalEnv = Evaluator ask

localEnv :: (EnvironmentFor value -> EnvironmentFor value) -> Evaluator effects value a -> Evaluator effects value a
localEnv f a = Evaluator (local f (runEvaluator a))


getStore :: Evaluator effects value (StoreFor value)
getStore = Evaluator get

modifyStore :: (StoreFor value -> StoreFor value) -> Evaluator effects value ()
modifyStore f = Evaluator (modify f)


getModuleTable :: Evaluator effects value (Linker value)
getModuleTable = Evaluator get

modifyModuleTable :: (Linker value -> Linker value) -> Evaluator effects value ()
modifyModuleTable f = Evaluator (modify f)


askModuleEvaluatorTable :: Evaluator effects value (Linker (Evaluator effects value value))
askModuleEvaluatorTable = Evaluator ask


data Evaluator effects value a
  = Evaluator
    { runEvaluator :: Members '[ Fail
                               , Reader (EnvironmentFor value)
                               , State  (EnvironmentFor value)
                               , State  (StoreFor value)
                               , Reader (Linker (Evaluator effects value value))
                               , State  (Linker value)
                               ] effects
                   => Eff effects a
    }


instance Functor (Evaluator effects value) where
  fmap f (Evaluator run) = Evaluator (fmap f run)

instance Applicative (Evaluator effects value) where
  pure = Evaluator . pure

  Evaluator runF <*> Evaluator runA = Evaluator (runF <*> runA)

instance Member NonDetEff effects => Alternative (Evaluator effects value) where
  empty = Evaluator empty

  Evaluator runA <|> Evaluator runB = Evaluator (runA <|> runB)

instance Monad (Evaluator effects value) where
  return = pure

  Evaluator runA >>= f = Evaluator (runA >>= runEvaluator . f)

instance MonadFail (Evaluator effects value) where
  fail s = Evaluator (fail s)

instance Member Fresh effects => MonadFresh (Evaluator effects value) where
  fresh = Evaluator fresh

  reset t = Evaluator (reset t)
