{-# LANGUAGE KindSignatures, MultiParamTypeClasses, TypeFamilies #-}
module Control.Abstract.Analysis
( MonadAnalysis(..)
, evaluateTerm
, liftAnalyze
, liftEvaluate
, module X
, Subterm(..)
, SubtermAlgebra
) where

import Control.Abstract.Evaluator as X
import Control.Effect as X
import Control.Monad.Effect.Fail as X
import Control.Monad.Effect.Reader as X
import Control.Monad.Effect.State as X
import Data.Coerce
import Prologue

-- | A 'Monad' in which one can evaluate some specific term type to some specific value type.
--
--   This typeclass is left intentionally unconstrained to avoid circular dependencies between it and other typeclasses.
class (MonadEvaluator effects m, Recursive (TermFor m)) => MonadAnalysis effects m where
  -- | Analyze a term using the semantics of the current analysis. This should generally only be called by definitions of 'evaluateTerm' and 'analyzeTerm' in this or other instances.
  analyzeTerm :: SubtermAlgebra (Base (TermFor m)) (TermFor m) (m effects (ValueFor m))

  evaluateModule :: TermFor m -> m effects (ValueFor m)
  evaluateModule = evaluateTerm

-- | Evaluate a term to a value using the semantics of the current analysis.
--
--   This should always be called when e.g. evaluating the bodies of closures instead of explicitly folding either 'eval' or 'analyzeTerm' over subterms, except in 'MonadAnalysis' instances themselves. On the other hand, top-level evaluation should be performed using 'evaluateModule'.
evaluateTerm :: MonadAnalysis effects m => TermFor m -> m effects (ValueFor m)
evaluateTerm = foldSubterms analyzeTerm

liftAnalyze :: ( term ~ TermFor (  m)
               , term ~ TermFor (t m)
               , value ~ ValueFor (  m)
               , value ~ ValueFor (t m)
               , Coercible (  m effects value) (t m effects value)
               , Coercible (t m effects value) (  m effects value)
               , Functor (Base term)
               )
            => SubtermAlgebra (Base term) term (  m effects value)
            -> SubtermAlgebra (Base term) term (t m effects value)
liftAnalyze analyze term = coerce (analyze (second coerce <$> term))

liftEvaluate :: ( term ~ TermFor (  m)
                , term ~ TermFor (t m)
                , value ~ ValueFor (  m)
                , value ~ ValueFor (t m)
                , Coercible (m effects value) (t m effects value)
                )
             => (term ->   m effects value)
             -> (term -> t m effects value)
liftEvaluate evaluate = coerce . evaluate
