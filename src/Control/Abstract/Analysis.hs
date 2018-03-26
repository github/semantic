{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For runAnalysis
module Control.Abstract.Analysis
( MonadAnalysis(..)
, liftAnalyze
, runAnalysis
, module X
, Subterm(..)
, SubtermAlgebra
) where

import Control.Abstract.Addressable as X
import Control.Abstract.Evaluator as X
import Control.Abstract.Value as X
import Control.Effect as X
import Control.Effect.Fresh as X
import Control.Effect.NonDet as X
import qualified Control.Monad.Effect as Effect
import Control.Monad.Effect.Fail as X
import Control.Monad.Effect.Reader as X
import Control.Monad.Effect.State as X
import Data.Abstract.Module
import Data.Coerce
import Data.Type.Coercion
import Prologue

-- | A 'Monad' in which one can evaluate some specific term type to some specific value type.
--
--   This typeclass is left intentionally unconstrained to avoid circular dependencies between it and other typeclasses.
class MonadEvaluator term value m => MonadAnalysis term value m where
  -- | The effects necessary to run the analysis. Analyses which are composed on top of (wrap) other analyses should include the inner analyses 'RequiredEffects' in their own list.
  type family RequiredEffects term value m :: [* -> *]

  -- | Analyze a term using the semantics of the current analysis.
  analyzeTerm :: (Base term (Subterm term (outer value)) -> m value)
              -> (Base term (Subterm term (outer value)) -> m value)

  -- | Analyze a module using the semantics of the current analysis. This should generally only be called by 'evaluateModule' and by definitions of 'analyzeModule' in instances for composite analyses.
  analyzeModule :: (Module (Subterm term (outer value)) -> m value)
                -> (Module (Subterm term (outer value)) -> m value)

  -- | Isolate the given action with an empty global environment and exports.
  isolate :: m a -> m a
  isolate = withEnv mempty . withExports mempty


-- | Lift a 'SubtermAlgebra' for an underlying analysis into a containing analysis. Use this when defining an analysis which can be composed onto other analyses to ensure that a call to 'analyzeTerm' occurs in the inner analysis and not the outer one.
liftAnalyze :: Coercible (  m term value effects value) (t m term value (effects :: [* -> *]) value)
            => ((base (Subterm term (outer value)) ->   m term value effects value) -> (base (Subterm term (outer value)) ->   m term value effects value))
            -> ((base (Subterm term (outer value)) -> t m term value effects value) -> (base (Subterm term (outer value)) -> t m term value effects value))
liftAnalyze analyze recur term = coerce (analyze (coerceWith (sym Coercion) . recur) term)


-- | Run an analysis, performing its effects and returning the result alongside any state.
--
--   This enables us to refer to the analysis type as e.g. @Analysis1 (Analysis2 Evaluating) Term Value@ without explicitly mentioning its effects (which are inferred to be simply its 'RequiredEffects').
runAnalysis :: ( Effectful m
               , RunEffects effects a
               , RequiredEffects term value (m effects) ~ effects
               , MonadAnalysis term value (m effects)
               )
            => m effects a
            -> Final effects a
runAnalysis = Effect.run . runEffects . lower
