{-# LANGUAGE DataKinds, RankNTypes, UndecidableInstances #-}
module Analysis.Abstract.Evaluator where

import Control.Monad.Effect
import Control.Monad.Effect.Fail
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
