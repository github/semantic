{-# LANGUAGE FunctionalDependencies #-}
module Control.Abstract.Analysis where

class Monad m => MonadAnalysis term value m | m -> term, m -> value where
  evaluateTerm :: term
               -> m value
