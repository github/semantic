{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Abstract.Analysis where

class Monad m => MonadAnalysis term value m where
  evaluateTerm :: term
               -> m value
