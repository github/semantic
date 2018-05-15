{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Abstract.TermEvaluator
( TermEvaluator(..)
, module X
) where

import Control.Abstract.Evaluator
import Control.Monad.Effect           as X
import Control.Monad.Effect.Fail      as X
import Control.Monad.Effect.Fresh     as X
import Control.Monad.Effect.NonDet    as X
import Control.Monad.Effect.Reader    as X
import Control.Monad.Effect.Resumable as X
import Control.Monad.Effect.State     as X
import Control.Monad.Effect.Trace     as X

newtype TermEvaluator term location value effects a = TermEvaluator { runTermEvaluator :: Evaluator location value effects a }
  deriving (Applicative, Effectful, Functor, Monad)
