{-# LANGUAGE ConstraintKinds, DefaultSignatures, GADTs, UndecidableInstances #-}
module Data.Abstract.Evaluatable
( module X
, MonadEvaluatable
, Evaluatable(..)
, Unspecialized(..)
, EvalError(..)
, evaluateTerm
, evaluateModule
, withModules
, evaluateModules
, throwEvalError
, require
, load
) where

import           Control.Abstract.Addressable as X
import           Control.Abstract.Analysis as X
import qualified Data.Abstract.Environment as Env
import qualified Data.Abstract.Exports as Exports
import           Data.Abstract.FreeVariables as X
import           Data.Abstract.Module
import           Data.Abstract.ModuleTable as ModuleTable
import           Data.Semigroup.App
import           Data.Semigroup.Foldable
import           Data.Term
import           Prelude hiding (fail)
import           Prologue

type MonadEvaluatable term value m =
  ( Evaluatable (Base term)
  , FreeVariables term
  , MonadAddressable (LocationFor value) value m
  , MonadAnalysis term value m
  , MonadThrow (Unspecialized value) m
  , MonadThrow (ValueExc value) m
  , MonadThrow (EvalError term value) m
  , MonadValue value m
  , Recursive term
  , Show (LocationFor value)
  )

data EvalError term value resume where
  FreeVariableError :: Prelude.String -> EvalError term value value
  LoadError         :: ModuleName -> EvalError term value [Module term]

deriving instance Eq (EvalError term a b)
deriving instance Show (EvalError term a b)
instance Show1 (EvalError term value) where
  liftShowsPrec _ _ = showsPrec

throwEvalError :: MonadEvaluatable term value m =>  EvalError term value resume -> m resume
throwEvalError = throwException

data Unspecialized a b where
  Unspecialized :: { getUnspecialized :: Prelude.String } -> Unspecialized value value

instance Eq1 (Unspecialized a) where
  liftEq _ (Unspecialized a) (Unspecialized b) = a == b

deriving instance Eq (Unspecialized a b)
deriving instance Show (Unspecialized a b)
instance Show1 (Unspecialized a) where
  liftShowsPrec _ _ = showsPrec

-- | The 'Evaluatable' class defines the necessary interface for a term to be evaluated. While a default definition of 'eval' is given, instances with computational content must implement 'eval' to perform their small-step operational semantics.
class Evaluatable constr where
  eval :: MonadEvaluatable term value m
       => SubtermAlgebra constr term (m value)
  default eval :: (MonadThrow (Unspecialized value) m, Show1 constr) => SubtermAlgebra constr term (m value)
  eval expr = throwException (Unspecialized ("Eval unspecialized for " ++ liftShowsPrec (const (const id)) (const id) 0 expr ""))


-- Instances

-- | If we can evaluate any syntax which can occur in a 'Union', we can evaluate the 'Union'.
instance Apply Evaluatable fs => Evaluatable (Union fs) where
  eval = Prologue.apply (Proxy :: Proxy Evaluatable) eval

-- | Evaluating a 'TermF' ignores its annotation, evaluating the underlying syntax.
instance Evaluatable s => Evaluatable (TermF s a) where
  eval = eval . termFOut

--- | '[]' is treated as an imperative sequence of statements/declarations s.t.:
---
---   1. Each statement’s effects on the store are accumulated;
---   2. Each statement can affect the environment of later statements (e.g. by 'modify'-ing the environment); and
---   3. Only the last statement’s return value is returned.
instance Evaluatable [] where
  -- 'nonEmpty' and 'foldMap1' enable us to return the last statement’s result instead of 'unit' for non-empty lists.
  eval = maybe unit (runApp . foldMap1 (App . subtermValue)) . nonEmpty

-- | Require/import another module by name and return it's environment and value.
--
-- Looks up the term's name in the cache of evaluated modules first, returns if found, otherwise loads/evaluates the module.
require :: MonadEvaluatable term value m
        => ModuleName
        -> m (EnvironmentFor value, value)
require name = getModuleTable >>= maybe (load name) pure . moduleTableLookup name

-- | Load another module by name and return it's environment and value.
--
-- Always loads/evaluates.
load :: MonadEvaluatable term value m
     => ModuleName
     -> m (EnvironmentFor value, value)
load name = askModuleTable >>= maybe notFound pure . moduleTableLookup name >>= evalAndCache
  where
    notFound = throwEvalError (LoadError name)

    evalAndCache []     = (,) <$> pure mempty <*> unit
    evalAndCache [x]    = evalAndCache' x
    evalAndCache (x:xs) = do
      (env, _) <- evalAndCache' x
      (env', v') <- evalAndCache xs
      pure (env <> env', v')

    evalAndCache' x = do
      v <- evaluateModule x
      env <- filterEnv <$> getExports <*> getEnv
      modifyModuleTable (moduleTableInsert name (env, v))
      pure (env, v)

    -- TODO: If the set of exports is empty because no exports have been
    -- defined, do we export all terms, or no terms? This behavior varies across
    -- languages. We need better semantics rather than doing it ad-hoc.
    filterEnv :: Exports.Exports l a -> Env.Environment l a -> Env.Environment l a
    filterEnv ports env
      | Exports.null ports = env
      | otherwise = Exports.toEnvironment ports <> Env.overwrite (Exports.aliases ports) env


-- | Evaluate a term to a value using the semantics of the current analysis.
--
--   This should always be called when e.g. evaluating the bodies of closures instead of explicitly folding either 'eval' or 'analyzeTerm' over subterms, except in 'MonadAnalysis' instances themselves. On the other hand, top-level evaluation should be performed using 'evaluateModule'.
evaluateTerm :: MonadEvaluatable term value m
             => term
             -> m value
evaluateTerm = foldSubterms (analyzeTerm eval)

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
evaluateModules []     = fail "evaluateModules: empty list"
evaluateModules (m:ms) = withModules ms (evaluateModule m)
