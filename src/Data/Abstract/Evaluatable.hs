{-# LANGUAGE ConstraintKinds, DefaultSignatures, GADTs, RankNTypes, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
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
, isolate
, Modules
) where

import Control.Abstract as X hiding (Goto(..), LoopControl(..), Modules(..), Return(..))
import Control.Abstract.Evaluator (LoopControl, Return(..))
import Control.Abstract.Goto (Goto(..))
import Control.Abstract.Modules (Modules(..))
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
  ( AbstractValue location value effects
  , Addressable location effects
  , Declarations term
  , FreeVariables term
  , Members '[ LoopControl value
             , Modules location value
             , Reader (Environment location value)
             , Reader ModuleInfo
             , Reader PackageInfo
             , Resumable (AddressError location value)
             , Resumable (EnvironmentError value)
             , Resumable (EvalError value)
             , Resumable ResolutionError
             , Resumable (Unspecialized value)
             , Return value
             , State (Environment location value)
             , State (Exports location value)
             , State (Heap location value)
             , Trace
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
evaluateInScopedEnv :: ( AbstractValue location value effects
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


traceResolve :: (Show a, Show b, Member Trace effects) => a -> b -> Evaluator location term value effects ()
traceResolve name path = traceE ("resolved " <> show name <> " -> " <> show path)


-- | Evaluate a given package.
evaluatePackageWith :: ( Evaluatable (Base term)
                       , EvaluatableConstraints location term value inner
                       , Members '[ Fail
                                  , Reader (Environment location value)
                                  , Resumable (LoadError term)
                                  , State (Environment location value)
                                  , State (Exports location value)
                                  , State (ModuleTable (Maybe (Environment location value, value)))
                                  , Trace
                                  ] outer
                       , Recursive term
                       , inner ~ (Goto inner' value ': inner')
                       , inner' ~ (LoopControl value ': Return value ': Reader ModuleInfo ': Modules location value ': Reader PackageInfo ': outer)
                       )
                    => (SubtermAlgebra Module term (Evaluator location term value inner value) -> SubtermAlgebra Module term (Evaluator location term value inner value))
                    -> (SubtermAlgebra (Base term) term (Evaluator location term value inner value) -> SubtermAlgebra (Base term) term (Evaluator location term value inner value))
                    -> Package term
                    -> Evaluator location term value outer [value]
evaluatePackageWith perModule perTerm = runReader . packageInfo <*> evaluatePackageBodyWith perModule perTerm . packageBody

-- | Evaluate a given package body (module table and entry points).
evaluatePackageBodyWith :: forall location term value inner inner' outer
                        .  ( Evaluatable (Base term)
                           , EvaluatableConstraints location term value inner
                           , Members '[ Fail
                                      , Reader (Environment location value)
                                      , Resumable (LoadError term)
                                      , State (Environment location value)
                                      , State (Exports location value)
                                      , State (ModuleTable (Maybe (Environment location value, value)))
                                      , Trace
                                      ] outer
                           , Recursive term
                           , inner ~ (Goto inner' value ': inner')
                           , inner' ~ (LoopControl value ': Return value ': Reader ModuleInfo ': Modules location value ': outer)
                           )
                        => (SubtermAlgebra Module term (Evaluator location term value inner value) -> SubtermAlgebra Module term (Evaluator location term value inner value))
                        -> (SubtermAlgebra (Base term) term (Evaluator location term value inner value) -> SubtermAlgebra (Base term) term (Evaluator location term value inner value))
                        -> PackageBody term
                        -> Evaluator location term value outer [value]
evaluatePackageBodyWith perModule perTerm body
  = runReader (packageModules body)
  . withPrelude (packagePrelude body)
  $ traverse (uncurry evaluateEntryPoint) (ModuleTable.toPairs (packageEntryPoints body))
  where evalModule m
          = runInModule (moduleInfo m)
          . perModule (subtermValue . moduleBody)
          . fmap (Subterm <*> foldSubterms (perTerm eval))
          $ m
        runInModule info
          = runModules evalModule
          . runReader info
          . runReturn
          . runLoopControl
          . fmap fst
          . runGoto lowerBound

        evaluateEntryPoint :: ModulePath -> Maybe Name -> Evaluator location term value (Reader (ModuleTable [Module term]) ': outer) value
        evaluateEntryPoint m sym = runInModule (ModuleInfo m) $ do
          v <- maybe unit (pure . snd) <$> require m
          maybe v ((`call` []) <=< variable) sym

        withPrelude Nothing a = a
        withPrelude (Just prelude) a = do
          preludeEnv <- evalModule prelude *> getEnv
          withDefaultEnvironment preludeEnv a

-- | Isolate the given action with an empty global environment and exports.
isolate :: Members '[State (Environment location value), State (Exports location value)] effects => Evaluator location term value effects a -> Evaluator location term value effects a
isolate = withEnv lowerBound . withExports lowerBound
