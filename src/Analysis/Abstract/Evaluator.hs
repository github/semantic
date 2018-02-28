{-# LANGUAGE DataKinds, RankNTypes, UndecidableInstances #-}
module Analysis.Abstract.Evaluator where

import Control.Applicative
import Control.Monad.Effect
import Control.Monad.Effect.Fail
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Value

data Evaluator effects value a
  = Evaluator
    { runEvaluator :: Members '[ Fail
                               , Reader (EnvironmentFor value)
                               , State  (EnvironmentFor value)
                               , State  (StoreFor value)
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
