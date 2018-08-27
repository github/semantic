{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Abstract.TermEvaluator
( TermEvaluator(..)
, raiseHandler
, module X
) where

import Control.Abstract.Evaluator
import Control.Monad.Effect           as X
import Control.Monad.Effect.Fresh     as X
import Control.Monad.Effect.NonDet    as X
import Control.Monad.Effect.Reader    as X
import Control.Monad.Effect.Resumable as X
import Control.Monad.Effect.State     as X
import Control.Monad.Effect.Trace     as X
import Control.Monad.IO.Class
import Prologue

-- | Evaluators specialized to some specific term type.
--
--   This is used to constrain the term type so that inference for analyses can resolve it correctly, but should not be used for any of the term-agonstic machinery like builtins, Evaluatable instances, the mechanics of the heap & environment, etc.
newtype TermEvaluator term address value effects a = TermEvaluator { runTermEvaluator :: Evaluator address value effects a }
  deriving (Applicative, Effectful, Functor, Monad)

deriving instance Member NonDet effects => Alternative (TermEvaluator term address value effects)
deriving instance Member (Lift IO) effects => MonadIO (TermEvaluator term address value effects)


raiseHandler :: (Evaluator address value effects a -> Evaluator address value effects' a') -> (TermEvaluator term address value effects a -> TermEvaluator term address value effects' a')
raiseHandler f = TermEvaluator . f . runTermEvaluator
