{-# LANGUAGE ConstraintKinds, DefaultSignatures, GADTs, RankNTypes, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Evaluatable
( module X
, Evaluatable(..)
, evaluatePackageWith
, isolate
, traceResolve
-- | Effects
, EvalError(..)
, throwEvalError
, runEvalError
, runEvalErrorWith
, Unspecialized(..)
, runUnspecialized
, runUnspecializedWith
, Cell
) where

import Control.Abstract
import Control.Abstract.Context as X
import Control.Abstract.Environment as X hiding (runEnvironmentError, runEnvironmentErrorWith)
import Control.Abstract.Evaluator as X hiding (LoopControl(..), Return(..), catchLoopControl, runLoopControl, catchReturn, runReturn)
import Control.Abstract.Exports as X
import Control.Abstract.Heap as X hiding (AddressError(..), runAddressError, runAddressErrorWith)
import Control.Abstract.Modules as X (Modules, ResolutionError(..), load, lookupModule, listModulesInDir, require, resolve)
import Control.Abstract.Value as X
import Data.Abstract.Declarations as X
import Data.Abstract.Environment as X
import Data.Abstract.Exports as Exports
import Data.Abstract.FreeVariables as X
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Abstract.Name as X
import Data.Abstract.Package as Package
import Data.Abstract.Ref as X
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
  eval :: ( EvaluatableConstraints address term value effects
          , Member Fail effects
          )
       => SubtermAlgebra constr term (Evaluator address value effects (ValueRef value))
  default eval :: (Member (Resumable (Unspecialized value)) effects, Show1 constr) => SubtermAlgebra constr term (Evaluator address value effects (ValueRef value))
  eval expr = throwResumable (Unspecialized ("Eval unspecialized for " ++ liftShowsPrec (const (const id)) (const id) 0 expr ""))

type EvaluatableConstraints address term value effects =
  ( AbstractValue address value effects
  , Declarations term
  , FreeVariables term
  , Member (Allocator address value) effects
  , Member (LoopControl value) effects
  , Member (Modules address value) effects
  , Member (Reader (Environment address)) effects
  , Member (Reader ModuleInfo) effects
  , Member (Reader PackageInfo) effects
  , Member (Reader Span) effects
  , Member (Resumable (EnvironmentError address)) effects
  , Member (Resumable EvalError) effects
  , Member (Resumable ResolutionError) effects
  , Member (Resumable (Unspecialized value)) effects
  , Member (Return value) effects
  , Member (State (Environment address)) effects
  , Member (State (Exports address)) effects
  , Member (State (Heap address (Cell address) value)) effects
  , Member Trace effects
  , Ord address
  , Reducer value (Cell address value)
  )


-- | Evaluate a given package.
evaluatePackageWith :: forall address term value inner outer
                    -- FIXME: It’d be nice if we didn’t have to mention 'Addressable' here at all, but 'Located' addresses require knowledge of 'currentModule' to run. Can we fix that? If not, can we factor this effect list out?
                    .  ( Addressable address (Reader ModuleInfo ': Modules address value ': Reader Span ': Reader PackageInfo ': outer)
                       , Evaluatable (Base term)
                       , EvaluatableConstraints address term value inner
                       , Member Fail outer
                       , Member Fresh outer
                       , Member (Reader (Environment address)) outer
                       , Member (Resumable (AddressError address value)) outer
                       , Member (Resumable (LoadError address value)) outer
                       , Member (State (Environment address)) outer
                       , Member (State (Exports address)) outer
                       , Member (State (Heap address (Cell address) value)) outer
                       , Member (State (ModuleTable (Maybe (Environment address, value)))) outer
                       , Member Trace outer
                       , Recursive term
                       , inner ~ (LoopControl value ': Return value ': Allocator address value ': Reader ModuleInfo ': Modules address value ': Reader Span ': Reader PackageInfo ': outer)
                       )
                    => (SubtermAlgebra Module      term (TermEvaluator term address value inner value)            -> SubtermAlgebra Module      term (TermEvaluator term address value inner value))
                    -> (SubtermAlgebra (Base term) term (TermEvaluator term address value inner (ValueRef value)) -> SubtermAlgebra (Base term) term (TermEvaluator term address value inner (ValueRef value)))
                    -> Package term
                    -> TermEvaluator term address value outer [value]
evaluatePackageWith analyzeModule analyzeTerm package
  = runReader (packageInfo package)
  . runReader lowerBound
  . runReader (packageModules (packageBody package))
  . withPrelude (packagePrelude (packageBody package))
  . raiseHandler (runModules (runTermEvaluator . evalModule))
  $ traverse (uncurry evaluateEntryPoint) (ModuleTable.toPairs (packageEntryPoints (packageBody package)))
  where
        evalModule m
          = pairValueWithEnv
          . runInModule (moduleInfo m)
          . analyzeModule (subtermRef . moduleBody)
          $ evalTerm <$> m
        evalTerm term = Subterm term (TermEvaluator (value =<< runTermEvaluator (foldSubterms (analyzeTerm (TermEvaluator . eval . fmap (second runTermEvaluator))) term)))

        runInModule info
          = runReader info
          . raiseHandler runAllocator
          . raiseHandler runReturn
          . raiseHandler runLoopControl

        evaluateEntryPoint :: ModulePath -> Maybe Name -> TermEvaluator term address value (Modules address value ': Reader Span ': Reader PackageInfo ': outer) value
        evaluateEntryPoint m sym = runInModule (ModuleInfo m) . TermEvaluator $ do
          v <- maybe unit snd <$> require m
          maybe (pure v) ((`call` []) <=< variable) sym

        evalPrelude prelude = raiseHandler (runModules (runTermEvaluator . evalModule)) $ do
          _ <- runInModule moduleInfoFromCallStack (TermEvaluator (defineBuiltins $> unit))
          fst <$> evalModule prelude

        withPrelude Nothing a = a
        withPrelude (Just prelude) a = do
          preludeEnv <- evalPrelude prelude
          raiseHandler (withDefaultEnvironment preludeEnv) a

        -- TODO: If the set of exports is empty because no exports have been
        -- defined, do we export all terms, or no terms? This behavior varies across
        -- languages. We need better semantics rather than doing it ad-hoc.
        filterEnv ports env
          | Exports.null ports = env
          | otherwise          = Exports.toEnvironment ports `mergeEnvs` overwrite (Exports.aliases ports) env
        pairValueWithEnv action = flip (,) <$> action <*> (filterEnv <$> TermEvaluator getExports <*> TermEvaluator getEnv)


-- | Isolate the given action with an empty global environment and exports.
isolate :: (Member (State (Environment address)) effects, Member (State (Exports address)) effects) => Evaluator address value effects a -> Evaluator address value effects a
isolate = withEnv lowerBound . withExports lowerBound

traceResolve :: (Show a, Show b, Member Trace effects) => a -> b -> Evaluator address value effects ()
traceResolve name path = trace ("resolved " <> show name <> " -> " <> show path)


-- Effects

-- | The type of error thrown when failing to evaluate a term.
data EvalError return where
  FreeVariablesError :: [Name] -> EvalError Name
  -- Indicates that our evaluator wasn't able to make sense of these literals.
  IntegerFormatError  :: ByteString -> EvalError Integer
  FloatFormatError    :: ByteString -> EvalError Scientific
  RationalFormatError :: ByteString -> EvalError Rational
  DefaultExportError  :: EvalError ()
  ExportError         :: ModulePath -> Name -> EvalError ()

deriving instance Eq (EvalError return)
deriving instance Show (EvalError return)

instance Eq1 EvalError where
  liftEq _ (FreeVariablesError a) (FreeVariablesError b)   = a == b
  liftEq _ DefaultExportError DefaultExportError           = True
  liftEq _ (ExportError a b) (ExportError c d)             = (a == c) && (b == d)
  liftEq _ (IntegerFormatError a) (IntegerFormatError b)   = a == b
  liftEq _ (FloatFormatError a) (FloatFormatError b)       = a == b
  liftEq _ (RationalFormatError a) (RationalFormatError b) = a == b
  liftEq _ _ _                                             = False

instance Show1 EvalError where
  liftShowsPrec _ _ = showsPrec

throwEvalError :: (Effectful m, Member (Resumable EvalError) effects) => EvalError resume -> m effects resume
throwEvalError = throwResumable

runEvalError :: Effectful m => m (Resumable EvalError ': effects) a -> m effects (Either (SomeExc EvalError) a)
runEvalError = runResumable

runEvalErrorWith :: Effectful m => (forall resume . EvalError resume -> m effects resume) -> m (Resumable EvalError ': effects) a -> m effects a
runEvalErrorWith = runResumableWith


data Unspecialized a b where
  Unspecialized :: String -> Unspecialized value (ValueRef value)

deriving instance Eq (Unspecialized a b)
deriving instance Show (Unspecialized a b)

instance Eq1 (Unspecialized a) where
  liftEq _ (Unspecialized a) (Unspecialized b) = a == b

instance Show1 (Unspecialized a) where
  liftShowsPrec _ _ = showsPrec

runUnspecialized :: Effectful (m value) => m value (Resumable (Unspecialized value) ': effects) a -> m value effects (Either (SomeExc (Unspecialized value)) a)
runUnspecialized = runResumable

runUnspecializedWith :: Effectful (m value) => (forall resume . Unspecialized value resume -> m value effects resume) -> m value (Resumable (Unspecialized value) ': effects) a -> m value effects a
runUnspecializedWith = runResumableWith


-- Instances

-- | If we can evaluate any syntax which can occur in a 'Sum', we can evaluate the 'Sum'.
instance Apply Evaluatable fs => Evaluatable (Sum fs) where
  eval = apply @Evaluatable eval

-- | Evaluating a 'TermF' ignores its annotation, evaluating the underlying syntax.
instance Evaluatable s => Evaluatable (TermF s a) where
  eval = eval . termFOut

-- | '[]' is treated as an imperative sequence of statements/declarations s.t.:
--
--   1. Each statement’s effects on the store are accumulated;
--   2. Each statement can affect the environment of later statements (e.g. by 'modify'-ing the environment); and
--   3. Only the last statement’s return value is returned.
instance Evaluatable [] where
  -- 'nonEmpty' and 'foldMap1' enable us to return the last statement’s result instead of 'unit' for non-empty lists.
  eval = maybe (pure (Rval unit)) (runApp . foldMap1 (App . subtermRef)) . nonEmpty
