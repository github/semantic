{-# LANGUAGE KindSignatures #-}
module Control.Abstract.Analysis
( MonadAnalysis(..)
, liftAnalyze
, module X
) where

import Control.Abstract.Addressable as X
import Control.Abstract.Evaluator as X
import Control.Abstract.Value as X
import Control.Effect as X
import Control.Monad.Effect.Fail as X
import Control.Monad.Effect.Fresh as X
import Control.Monad.Effect.Internal as X (Eff, relay)
import Control.Monad.Effect.NonDet as X
import Control.Monad.Effect.Reader as X
import Control.Monad.Effect.State as X
import Control.Monad.Effect.Resumable as X
import Data.Abstract.Module
import Data.Coerce
import Data.Type.Coercion
import Prologue

-- | A 'Monad' in which one can evaluate some specific term type to some specific value type.
--
--   This typeclass is left intentionally unconstrained to avoid circular dependencies between it and other typeclasses.
class MonadEvaluator location term value effects m => MonadAnalysis location term value effects m where
  -- | Analyze a term using the semantics of the current analysis.
  analyzeTerm :: (Base term (Subterm term (outer effects value)) -> m effects value)
              -> (Base term (Subterm term (outer effects value)) -> m effects value)

  -- | Analyze a module using the semantics of the current analysis. This should generally only be called by 'evaluateModule' and by definitions of 'analyzeModule' in instances for composite analyses.
  analyzeModule :: (Module (Subterm term (outer effects value)) -> m effects value)
                -> (Module (Subterm term (outer effects value)) -> m effects value)

  -- | Isolate the given action with an empty global environment and exports.
  isolate :: m effects a -> m effects a
  isolate = withEnv mempty . withExports mempty


-- | Lift a 'SubtermAlgebra' for an underlying analysis into a containing analysis. Use this when defining an analysis which can be composed onto other analyses to ensure that a call to 'analyzeTerm' occurs in the inner analysis and not the outer one.
liftAnalyze :: Coercible (  m effects value) (t m (effects :: [* -> *]) value)
            => ((base (Subterm term (outer value)) ->   m effects value) -> (base (Subterm term (outer value)) ->   m effects value))
            -> ((base (Subterm term (outer value)) -> t m effects value) -> (base (Subterm term (outer value)) -> t m effects value))
liftAnalyze analyze recur term = coerce (analyze (coerceWith (sym Coercion) . recur) term)
