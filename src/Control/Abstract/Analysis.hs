{-# LANGUAGE FunctionalDependencies #-}
module Control.Abstract.Analysis where

-- | A 'Monad' in which one can evaluate some specific term type to some specific value type.
--
--   This typeclass is left intentionally unconstrained to avoid circular dependencies between it and other typeclasses.
class Monad m => MonadAnalysis term value m | m -> term, m -> value where
  evaluateTerm :: term -> m value
