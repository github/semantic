{-# LANGUAGE DataKinds, FunctionalDependencies, KindSignatures, TypeFamilies #-}
module Control.Abstract.Analysis
( MonadAnalysis(..)
, evaluateTerm
, liftAnalyze
, liftEvaluate
, runAnalysis
, module X
, Subterm(..)
, SubtermAlgebra
) where

import Control.Abstract.Evaluator as X
import Control.Effect as X
import qualified Control.Monad.Effect as Effect
import Control.Monad.Effect.Fail as X
import Control.Monad.Effect.Reader as X
import Control.Monad.Effect.State as X
import Data.Coerce
import Prologue

-- | A 'Monad' in which one can evaluate some specific term type to some specific value type.
--
--   This typeclass is left intentionally unconstrained to avoid circular dependencies between it and other typeclasses.
class (MonadEvaluator term value m, Recursive term) => MonadAnalysis term value m | m -> term, m -> value where
  type family RequiredEffects term value m :: [* -> *]
  -- | Analyze a term using the semantics of the current analysis. This should generally only be called by definitions of 'evaluateTerm' and 'analyzeTerm' in this or other instances.
  analyzeTerm :: SubtermAlgebra (Base term) term (m value)

  evaluateModule :: term -> m value
  evaluateModule = evaluateTerm

-- | Evaluate a term to a value using the semantics of the current analysis.
--
--   This should always be called when e.g. evaluating the bodies of closures instead of explicitly folding either 'eval' or 'analyzeTerm' over subterms, except in 'MonadAnalysis' instances themselves. On the other hand, top-level evaluation should be performed using 'evaluateModule'.
evaluateTerm :: MonadAnalysis term value m => term -> m value
evaluateTerm = foldSubterms analyzeTerm

liftAnalyze :: ( Coercible (  m term value (effects :: [* -> *]) value) (t m term value effects value)
               , Coercible (t m term value effects value) (  m term value effects value)
               , Functor (Base term)
               )
            => SubtermAlgebra (Base term) term (  m term value effects value)
            -> SubtermAlgebra (Base term) term (t m term value effects value)
liftAnalyze analyze term = coerce (analyze (second coerce <$> term))

liftEvaluate :: ( Coercible (m term value (effects :: [* -> *]) value) (t m term value effects value)
                )
             => (term ->   m term value effects value)
             -> (term -> t m term value effects value)
liftEvaluate evaluate = coerce . evaluate


runAnalysis :: (Effectful m, RunEffects effects a, RequiredEffects term value (m effects) ~ effects, MonadAnalysis term value (m effects)) => m effects a -> Final effects a
runAnalysis = Effect.run . runEffects . lower
