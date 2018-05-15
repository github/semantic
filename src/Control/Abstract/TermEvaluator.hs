{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Abstract.TermEvaluator where

import Control.Abstract.Evaluator

newtype TermEvaluator term location value effects a = TermEvaluator { runTermEvaluator :: Evaluator location value effects a }
  deriving (Applicative, Effectful, Functor, Monad)
