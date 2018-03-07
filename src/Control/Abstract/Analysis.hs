{-# LANGUAGE DefaultSignatures, FunctionalDependencies #-}
module Control.Abstract.Analysis
( MonadAnalysis(..)
) where

import Prologue

-- | A 'Monad' in which one can evaluate some specific term type to some specific value type.
--
--   This typeclass is left intentionally unconstrained to avoid circular dependencies between it and other typeclasses.
class Monad m => MonadAnalysis term value m | m -> term, m -> value where
  -- | Analyze a term using the semantics of the current analysis. This should generally only be called by definitions of 'evaluateTerm' and 'analyzeTerm' in this or other instances.
  analyzeTerm :: SubtermAlgebra (Base term) term (m value)

  -- | Evaluate a term to a value using the semantics of the current analysis.
  --
  --   This should always be called instead of explicitly folding either 'eval' or 'analyzeTerm' over subterms, except in 'MonadAnalysis' instances themselves.
  evaluateTerm :: MonadAnalysis term value m => term -> m value
  default evaluateTerm :: (MonadAnalysis term value m, Recursive term) => term -> m value
  evaluateTerm = foldSubterms analyzeTerm
