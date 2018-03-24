{-# LANGUAGE ConstraintKinds, DefaultSignatures, RankNTypes, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For runAnalysis
module Control.Abstract.Analysis
( MonadAnalysis(..)
, evaluateTerm
, evaluateModule
, withModules
, evaluateModules
, liftAnalyze
, runAnalysis
, module X
, Subterm(..)
, SubtermAlgebra
, Evaluatable(..)
, MonadEvaluatable
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
import Data.Abstract.FreeVariables
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Coerce
import Data.Semigroup.App
import Data.Semigroup.Foldable
import Data.Term
import Prelude hiding (fail)
import Prologue

-- | A 'Monad' in which one can evaluate some specific term type to some specific value type.
--
--   This typeclass is left intentionally unconstrained to avoid circular dependencies between it and other typeclasses.
class MonadEvaluator term value m => MonadAnalysis term value m where
  -- | The effects necessary to run the analysis. Analyses which are composed on top of (wrap) other analyses should include the inner analyses 'RequiredEffects' in their own list.
  type family RequiredEffects term value m :: [* -> *]

  -- | Analyze a term using the semantics of the current analysis. This should generally only be called by 'evaluateTerm' and by definitions of 'analyzeTerm' in instances for composite analyses.
  analyzeTerm :: (Base term (Subterm term (outer value)) -> m value) -> (Base term (Subterm term (outer value)) -> m value)

  -- | Analyze a module using the semantics of the current analysis. This should generally only be called by 'evaluateModule' and by definitions of 'analyzeModule' in instances for composite analyses.
  analyzeModule :: (Module (Subterm term (outer value)) -> m value) -> (Module (Subterm term (outer value)) -> m value)

  -- | Isolate the given action with an empty global environment and exports.
  isolate :: m a -> m a
  isolate = withEnv mempty . withExports mempty

-- | Evaluate a term to a value using the semantics of the current analysis.
--
--   This should always be called when e.g. evaluating the bodies of closures instead of explicitly folding either 'eval' or 'analyzeTerm' over subterms, except in 'MonadAnalysis' instances themselves. On the other hand, top-level evaluation should be performed using 'evaluateModule'.
evaluateTerm :: MonadEvaluatable term value m
             => term
             -> m value
evaluateTerm = foldSubterms (analyzeTerm eval)

type MonadEvaluatable term value m =
  ( Evaluatable (Base term)
  , FreeVariables term
  , MonadAddressable (LocationFor value) value m
  , MonadAnalysis term value m
  , MonadThrow Prelude.String value m
  , MonadValue value m
  , Recursive term
  , Show (LocationFor value)
  )

class Evaluatable constr where
  eval :: MonadEvaluatable term value m
       => SubtermAlgebra constr term (m value)
  default eval :: (MonadThrow Prelude.String value m, Show1 constr) => SubtermAlgebra constr term (m value)
  eval expr = throwException $ "Eval unspecialized for " ++ liftShowsPrec (const (const id)) (const id) 0 expr ""

-- | If we can evaluate any syntax which can occur in a 'Union', we can evaluate the 'Union'.
instance Apply Evaluatable fs => Evaluatable (Union fs) where
  eval = Prologue.apply (Proxy :: Proxy Evaluatable) eval

-- | Evaluating a 'TermF' ignores its annotation, evaluating the underlying syntax.
instance Evaluatable s => Evaluatable (TermF s a) where
  eval = eval . termFOut


-- | Evaluate a (root-level) term to a value using the semantics of the current analysis. This should be used to evaluate single-term programs, or (via 'evaluateModules') the entry point of multi-term programs.
evaluateModule :: MonadEvaluatable term value m
               => Module term
               -> m value
evaluateModule m = analyzeModule (subtermValue . moduleBody) (fmap (Subterm <*> evaluateTerm) m)


-- | Run an action with the a list of 'Module's available for imports.
withModules :: MonadEvaluatable term value m
            => [Module term]
            -> m a
            -> m a
withModules = localModuleTable . const . ModuleTable.fromList

-- | Evaluate with a list of modules in scope, taking the head module as the entry point.
evaluateModules :: MonadEvaluatable term value m
                => [Module term]
                -> m value
evaluateModules [] = fail "evaluateModules: empty list"
evaluateModules (m:ms) = withModules ms (evaluateModule m)


-- | Lift a 'SubtermAlgebra' for an underlying analysis into a containing analysis. Use this when defining an analysis which can be composed onto other analyses to ensure that a call to 'analyzeTerm' occurs in the inner analysis and not the outer one.
liftAnalyze :: ( Coercible (  m term value effects value) (t m term value (effects :: [* -> *]) value)
               , Coercible (t m term value effects value) (  m term value  effects              value)
               )
            => ((base (Subterm term (outer value)) ->   m term value effects value) -> (base (Subterm term (outer value)) ->   m term value effects value))
            -> ((base (Subterm term (outer value)) -> t m term value effects value) -> (base (Subterm term (outer value)) -> t m term value effects value))
liftAnalyze analyze recur term = coerce (analyze (coerce . recur) term)


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


instance Evaluatable [] where
  -- 'nonEmpty' and 'foldMap1' enable us to return the last statement’s result instead of 'unit' for non-empty lists.
  eval = maybe unit (runApp . foldMap1 (App . subtermValue)) . nonEmpty
