{-# LANGUAGE ConstraintKinds, DefaultSignatures, GADTs, UndecidableInstances #-}
module Data.Abstract.Evaluatable
( module X
, MonadEvaluatable
, Evaluatable(..)
, Unspecialized(..)
, LoadError(..)
, EvalError(..)
, ResolutionError(..)
, variable
, evaluateTerm
, evaluateModule
, evaluateModules
, evaluatePackage
, evaluatePackageBody
, throwLoadError
, resolve
, listModulesInDir
, require
, load
, pushOrigin
) where

import           Control.Abstract.Addressable as X
import           Control.Abstract.Analysis as X
import           Data.Abstract.Address
import           Data.Abstract.Environment as X
import qualified Data.Abstract.Exports as Exports
import           Data.Abstract.FreeVariables as X
import           Data.Abstract.Module
import           Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Origin (SomeOrigin, packageOrigin)
import           Data.Abstract.Package as Package
import           Data.Semigroup.App
import           Data.Semigroup.Foldable
import           Data.Semigroup.Reducer hiding (unit)
import           Data.Term
import           Prologue

type MonadEvaluatable location term value m =
  ( Evaluatable (Base term)
  , FreeVariables term
  , MonadAddressable location m
  , MonadAnalysis location term value m
  , MonadThrow (Unspecialized value) m
  , MonadThrow (ValueError location value) m
  , MonadThrow (LoadError term value) m
  , MonadThrow (EvalError value) m
  , MonadThrow (ResolutionError value) m
  , MonadValue location value m
  , Recursive term
  , Reducer value (Cell location value)
  , Show location
  )

-- | An error thrown when we can't resolve a module from a qualified name.
data ResolutionError value resume where
  RubyError :: String -> ResolutionError value ModulePath

deriving instance Eq (ResolutionError a b)
deriving instance Show (ResolutionError a b)
instance Show1 (ResolutionError value) where
  liftShowsPrec _ _ = showsPrec
instance Eq1 (ResolutionError value) where
  liftEq _ (RubyError a) (RubyError b) = a == b

-- | An error thrown when loading a module from the list of provided modules. Indicates we weren't able to find a module with the given name.
data LoadError term value resume where
  LoadError :: ModulePath -> LoadError term value [Module term]

deriving instance Eq (LoadError term a b)
deriving instance Show (LoadError term a b)
instance Show1 (LoadError term value) where
  liftShowsPrec _ _ = showsPrec
instance Eq1 (LoadError term a) where
  liftEq _ (LoadError a) (LoadError b) = a == b

-- | The type of error thrown when failing to evaluate a term.
data EvalError value resume where
  -- Indicates we weren't able to dereference a name from the evaluated environment.
  FreeVariableError :: Name -> EvalError value value

-- | Look up and dereference the given 'Name', throwing an exception for free variables.
variable :: MonadEvaluatable location term value m => Name -> m value
variable name = lookupWith deref name >>= maybeM (throwException (FreeVariableError name))

deriving instance Eq (EvalError a b)
deriving instance Show (EvalError a b)
instance Show1 (EvalError value) where
  liftShowsPrec _ _ = showsPrec
instance Eq1 (EvalError term) where
  liftEq _ (FreeVariableError a) (FreeVariableError b) = a == b

throwLoadError :: MonadEvaluatable location term value m => LoadError term value resume -> m resume
throwLoadError = throwException

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
  eval :: MonadEvaluatable location term value m
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

-- Resolve a list of module paths to a possible module table entry.
resolve :: MonadEvaluatable location term value m
        => [FilePath]
        -> m (Maybe ModulePath)
resolve names = do
  tbl <- askModuleTable
  pure $ find (`ModuleTable.member` tbl) names

listModulesInDir :: MonadEvaluatable location term value m
        => FilePath
        -> m [ModulePath]
listModulesInDir dir = ModuleTable.modulePathsInDir dir <$> askModuleTable

-- | Require/import another module by name and return it's environment and value.
--
-- Looks up the term's name in the cache of evaluated modules first, returns if found, otherwise loads/evaluates the module.
require :: MonadEvaluatable location term value m
        => ModulePath
        -> m (Environment location value, value)
require name = getModuleTable >>= maybeM (load name) . ModuleTable.lookup name

-- | Load another module by name and return it's environment and value.
--
-- Always loads/evaluates.
load :: MonadEvaluatable location term value m
     => ModulePath
     -> m (Environment location value, value)
load name = askModuleTable >>= maybeM notFound . ModuleTable.lookup name >>= evalAndCache
  where
    notFound = throwLoadError (LoadError name)

    evalAndCache []     = (,) mempty <$> unit
    evalAndCache [x]    = evalAndCache' x
    evalAndCache (x:xs) = do
      (env, _) <- evalAndCache' x
      (env', v') <- evalAndCache xs
      pure (env <> env', v')

    evalAndCache' x = do
      v <- evaluateModule x
      env <- filterEnv <$> getExports <*> getEnv
      modifyModuleTable (ModuleTable.insert name (env, v))
      pure (env, v)

    -- TODO: If the set of exports is empty because no exports have been
    -- defined, do we export all terms, or no terms? This behavior varies across
    -- languages. We need better semantics rather than doing it ad-hoc.
    filterEnv :: Exports.Exports l a -> Environment l a -> Environment l a
    filterEnv ports env
      | Exports.null ports = env
      | otherwise = Exports.toEnvironment ports <> overwrite (Exports.aliases ports) env


-- | Evaluate a term to a value using the semantics of the current analysis.
--
--   This should always be called when e.g. evaluating the bodies of closures instead of explicitly folding either 'eval' or 'analyzeTerm' over subterms, except in 'MonadAnalysis' instances themselves. On the other hand, top-level evaluation should be performed using 'evaluateModule'.
evaluateTerm :: MonadEvaluatable location term value m
             => term
             -> m value
evaluateTerm = foldSubterms (analyzeTerm eval)

-- | Evaluate a (root-level) term to a value using the semantics of the current analysis. This should be used to evaluate single-term programs, or (via 'evaluateModules') the entry point of multi-term programs.
evaluateModule :: MonadEvaluatable location term value m
               => Module term
               -> m value
evaluateModule m = analyzeModule (subtermValue . moduleBody) (fmap (Subterm <*> evaluateTerm) m)

-- | Evaluate with a list of modules in scope, taking the head module as the entry point.
evaluateModules :: MonadEvaluatable location term value m
                => [Module term]
                -> m value
evaluateModules = fmap Prelude.head . evaluatePackageBody . Package.fromModules

-- | Evaluate a given package.
evaluatePackage :: ( Effectful m
                   , Member (Reader (SomeOrigin term)) effects
                   , MonadEvaluatable location term value (m effects)
                   )
                => Package term
                -> m effects [value]
evaluatePackage p = pushOrigin (packageOrigin p) (evaluatePackageBody (packageBody p))

-- | Evaluate a given package body (module table and entry points).
evaluatePackageBody :: MonadEvaluatable location term value m
                    => PackageBody term
                    -> m [value]
evaluatePackageBody body = localModuleTable (<> packageModules body)
  (traverse evaluateEntryPoint (ModuleTable.toPairs (packageEntryPoints body)))
  where evaluateEntryPoint (m, sym) = do
          (_, v) <- require m
          maybe (pure v) ((`call` []) <=< variable) sym

-- | Push a 'SomeOrigin' onto the stack. This should be used to contextualize execution with information about the originating term, module, or package.
pushOrigin :: ( Effectful m
              , Member (Reader (SomeOrigin term)) effects
              )
           => SomeOrigin term
           -> m effects a
           -> m effects a
pushOrigin o = raise . local (<> o) . lower
