{-# LANGUAGE ConstraintKinds, DefaultSignatures, GADTs, RankNTypes, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Evaluatable
( module X
, Evaluatable(..)
, Unspecialized(..)
, runUnspecialized
, runUnspecializedWith
, EvalError(..)
, runEvalError
, runEvalErrorWith
, evaluateInScopedEnv
, evaluatePackageWith
, evaluatePackageBodyWith
, throwEvalError
, traceResolve
, LoadStack
, isolate
) where

import Control.Abstract as X hiding (LoopControl(..), Return(..))
import Control.Abstract.Evaluator (LoopControl, Return(..))
import Control.Monad.Effect as Eff
import Data.Abstract.Address
import Data.Abstract.Declarations as X
import Data.Abstract.Environment as X
import Data.Abstract.FreeVariables as X
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Abstract.Package as Package
import Data.Scientific (Scientific)
import Data.Semigroup.App
import Data.Semigroup.Foldable
import Data.Semigroup.Reducer hiding (unit)
import Data.Semilattice.Lower
import Data.Sum
import Data.Term
import Prologue

-- | The 'Evaluatable' class defines the necessary interface for a term to be evaluated. While a default definition of 'eval' is given, instances with computational content must implement 'eval' to perform their small-step operational semantics.
class Evaluatable constr where
  eval :: ( EvaluatableConstraints location term value effects
          , Member Fail effects
          )
       => SubtermAlgebra constr term (Evaluator location term value effects value)
  default eval :: (Member (Resumable (Unspecialized value)) effects, Show1 constr) => SubtermAlgebra constr term (Evaluator location term value effects value)
  eval expr = throwResumable (Unspecialized ("Eval unspecialized for " ++ liftShowsPrec (const (const id)) (const id) 0 expr ""))

type EvaluatableConstraints location term value effects =
  ( AbstractValue location term value effects
  , Addressable location effects
  , Declarations term
  , FreeVariables term
  , Members '[ EvalModule term value
             , LoopControl value
             , Reader (Environment location value)
             , Reader LoadStack
             , Reader ModuleInfo
             , Reader (ModuleTable [Module term])
             , Reader PackageInfo
             , Resumable (AddressError location value)
             , Resumable (EnvironmentError value)
             , Resumable (EvalError value)
             , Resumable (LoadError term)
             , Resumable ResolutionError
             , Resumable (Unspecialized value)
             , Return value
             , State (Environment location value)
             , State (Exports location value)
             , State (Heap location value)
             , State (ModuleTable (Environment location value, value))
             ] effects
  , Reducer value (Cell location value)
  )


-- | The type of error thrown when failing to evaluate a term.
data EvalError value resume where
  FreeVariablesError :: [Name] -> EvalError value Name
  -- Indicates that our evaluator wasn't able to make sense of these literals.
  IntegerFormatError  :: ByteString -> EvalError value Integer
  FloatFormatError    :: ByteString -> EvalError value Scientific
  RationalFormatError :: ByteString -> EvalError value Rational
  DefaultExportError  :: EvalError value ()
  ExportError         :: ModulePath -> Name -> EvalError value ()
  EnvironmentLookupError :: value -> EvalError value value

runEvalError :: Evaluator location term value (Resumable (EvalError value) ': effects) a -> Evaluator location term value effects (Either (SomeExc (EvalError value)) a)
runEvalError = raiseHandler runError

runEvalErrorWith :: (forall resume . EvalError value resume -> Evaluator location term value effects resume) -> Evaluator location term value (Resumable (EvalError value) ': effects) a -> Evaluator location term value effects a
runEvalErrorWith = runResumableWith

-- | Evaluate a term within the context of the scoped environment of 'scopedEnvTerm'.
--   Throws an 'EnvironmentLookupError' if @scopedEnvTerm@ does not have an environment.
evaluateInScopedEnv :: ( AbstractValue location term value effects
                       , Members '[ Resumable (EvalError value)
                                  , State (Environment location value)
                                  ] effects
                       )
                    => Evaluator location term value effects value
                    -> Evaluator location term value effects value
                    -> Evaluator location term value effects value
evaluateInScopedEnv scopedEnvTerm term = do
  value <- scopedEnvTerm
  scopedEnv <- scopedEnvironment value
  maybe (throwEvalError (EnvironmentLookupError value)) (flip localEnv term . mergeEnvs) scopedEnv

deriving instance Eq a => Eq (EvalError a b)
deriving instance Show a => Show (EvalError a b)
instance Show value => Show1 (EvalError value) where
  liftShowsPrec _ _ = showsPrec
instance Eq term => Eq1 (EvalError term) where
  liftEq _ (FreeVariablesError a) (FreeVariablesError b)   = a == b
  liftEq _ DefaultExportError DefaultExportError           = True
  liftEq _ (ExportError a b) (ExportError c d)             = (a == c) && (b == d)
  liftEq _ (IntegerFormatError a) (IntegerFormatError b)   = a == b
  liftEq _ (FloatFormatError a) (FloatFormatError b)       = a == b
  liftEq _ (RationalFormatError a) (RationalFormatError b) = a == b
  liftEq _ (EnvironmentLookupError a) (EnvironmentLookupError b) = a == b
  liftEq _ _ _                                             = False


throwEvalError :: Member (Resumable (EvalError value)) effects => EvalError value resume -> Evaluator location term value effects resume
throwEvalError = throwResumable


data Unspecialized a b where
  Unspecialized :: Prelude.String -> Unspecialized value value

instance Eq1 (Unspecialized a) where
  liftEq _ (Unspecialized a) (Unspecialized b) = a == b

deriving instance Eq (Unspecialized a b)
deriving instance Show (Unspecialized a b)
instance Show1 (Unspecialized a) where
  liftShowsPrec _ _ = showsPrec

runUnspecialized :: Evaluator location term value (Resumable (Unspecialized value) ': effects) a -> Evaluator location term value effects (Either (SomeExc (Unspecialized value)) a)
runUnspecialized = raiseHandler runError

runUnspecializedWith :: (forall resume . Unspecialized value resume -> Evaluator location term value effects resume) -> Evaluator location term value (Resumable (Unspecialized value) ': effects) a -> Evaluator location term value effects a
runUnspecializedWith = runResumableWith


-- Instances

-- | If we can evaluate any syntax which can occur in a 'Sum', we can evaluate the 'Sum'.
instance Apply Evaluatable fs => Evaluatable (Sum fs) where
  eval = apply @Evaluatable eval

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


traceResolve :: (Show a, Show b) => a -> b -> c -> c
traceResolve name path = trace ("resolved " <> show name <> " -> " <> show path)


-- | Evaluate a given package.
evaluatePackageWith :: ( Evaluatable (Base term)
                       , EvaluatableConstraints location term value termEffects
                       , Members '[ Fail
                                  , Reader (Environment location value)
                                  , State (Environment location value)
                                  ] effects
                       , Recursive term
                       , termEffects ~ (LoopControl value ': Return value ': EvalClosure term value ': moduleEffects)
                       , moduleEffects ~ (Reader ModuleInfo ': EvalModule term value ': packageBodyEffects)
                       , packageBodyEffects ~ (Reader LoadStack ': Reader (ModuleTable [Module term]) ': packageEffects)
                       , packageEffects ~ (Reader PackageInfo ': effects)
                       )
                    => (SubtermAlgebra Module term (Evaluator location term value moduleEffects value) -> SubtermAlgebra Module term (Evaluator location term value moduleEffects value))
                    -> (SubtermAlgebra (Base term) term (Evaluator location term value termEffects value) -> SubtermAlgebra (Base term) term (Evaluator location term value termEffects value))
                    -> Package term
                    -> Evaluator location term value effects [value]
evaluatePackageWith perModule perTerm = runReader . packageInfo <*> evaluatePackageBodyWith perModule perTerm . packageBody

-- | Evaluate a given package body (module table and entry points).
evaluatePackageBodyWith :: ( Evaluatable (Base term)
                           , EvaluatableConstraints location term value termEffects
                           , Members '[ Fail
                                      , Reader (Environment location value)
                                      , State (Environment location value)
                                      ] effects
                           , Recursive term
                           , termEffects ~ (LoopControl value ': Return value ': EvalClosure term value ': moduleEffects)
                           , moduleEffects ~ (Reader ModuleInfo ': EvalModule term value ': packageBodyEffects)
                           , packageBodyEffects ~ (Reader LoadStack ': Reader (ModuleTable [Module term]) ': effects)
                           )
                        => (SubtermAlgebra Module term (Evaluator location term value moduleEffects value) -> SubtermAlgebra Module term (Evaluator location term value moduleEffects value))
                        -> (SubtermAlgebra (Base term) term (Evaluator location term value termEffects value) -> SubtermAlgebra (Base term) term (Evaluator location term value termEffects value))
                        -> PackageBody term
                        -> Evaluator location term value effects [value]
evaluatePackageBodyWith perModule perTerm body
  = runReader (packageModules body)
  . runReader lowerBound
  . runEvalModule evalModule
  . withPrelude (packagePrelude body)
  $ traverse (uncurry evaluateEntryPoint) (ModuleTable.toPairs (packageEntryPoints body))
  where evalModule m
          = runEvalModule evalModule
          . runReader (moduleInfo m)
          . perModule (subtermValue . moduleBody)
          . fmap (Subterm <*> evalTerm)
          $ m
        evalTerm
          = runEvalClosure evalTerm
          . runReturn
          . runLoopControl
          . foldSubterms (perTerm eval)

        evaluateEntryPoint m sym = runReader (ModuleInfo m) . runEvalClosure evalTerm . runReturn . runLoopControl $ do
          v <- maybe unit (pure . snd) <$> require m
          maybe v ((`call` []) <=< variable) sym

        withPrelude Nothing a = a
        withPrelude (Just prelude) a = do
          preludeEnv <- evaluateModule prelude *> getEnv
          withDefaultEnvironment preludeEnv a

-- | Isolate the given action with an empty global environment and exports.
isolate :: Members '[State (Environment location value), State (Exports location value)] effects => Evaluator location term value effects a -> Evaluator location term value effects a
isolate = withEnv lowerBound . withExports lowerBound
