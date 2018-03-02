{-# LANGUAGE FunctionalDependencies #-}
module Control.Abstract.Analysis where

-- | A 'Monad' in which one can evaluate some specific term type to some specific value type.
--
--   This typeclass is left intentionally unconstrained to avoid circular dependencies between it and other typeclasses.
class Monad m => MonadAnalysis term value m | m -> term, m -> value where
  -- | Evaluate a term to a value using the semantics of the current analysis. This should always be used instead of explicitly folding 'eval' over subterms, except in 'MonadAnalysis' instances themselves.
  evaluateTerm :: term -> m value
