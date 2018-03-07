{-# LANGUAGE DefaultSignatures, KindSignatures, TypeFamilies #-}
module Control.Abstract.Analysis
( MonadAnalysis(..)
, AnalysisTerm
, AnalysisValue
, module X
, Subterm(..)
, SubtermAlgebra
) where

import Control.Effect as X
import Control.Monad.Effect.Fail as X
import Control.Monad.Effect.Reader as X
import Control.Monad.Effect.State as X
import Prologue

type family AnalysisTerm (m :: * -> *)
type family AnalysisValue (m :: * -> *)

-- | A 'Monad' in which one can evaluate some specific term type to some specific value type.
--
--   This typeclass is left intentionally unconstrained to avoid circular dependencies between it and other typeclasses.
class Monad m => MonadAnalysis m where
  -- | Analyze a term using the semantics of the current analysis. This should generally only be called by definitions of 'evaluateTerm' and 'analyzeTerm' in this or other instances.
  analyzeTerm :: SubtermAlgebra (Base (AnalysisTerm m)) (AnalysisTerm m) (m (AnalysisValue m))

  -- | Evaluate a term to a value using the semantics of the current analysis.
  --
  --   This should always be called instead of explicitly folding either 'eval' or 'analyzeTerm' over subterms, except in 'MonadAnalysis' instances themselves.
  evaluateTerm :: AnalysisTerm m -> m (AnalysisValue m)
  default evaluateTerm :: Recursive (AnalysisTerm m) => AnalysisTerm m -> m (AnalysisValue m)
  evaluateTerm = foldSubterms analyzeTerm
