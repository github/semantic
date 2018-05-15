{-# LANGUAGE ConstraintKinds, DefaultSignatures, GADTs, GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Evaluatable
( module X
, Evaluatable(..)
, Unspecialized(..)
, runUnspecialized
, runUnspecializedWith
, EvalError(..)
, runEvalError
, runEvalErrorWith
, subtermValue
, evaluateInScopedEnv
, evaluatePackageWith
, throwEvalError
, traceResolve
, builtin
, isolate
, Modules
) where

import Control.Abstract as X hiding (Goto(..), LoopControl(..), Modules(..), Return(..))
import Control.Abstract.Evaluator (LoopControl, Return(..))
import Control.Abstract.Goto (Goto(..))
import Control.Abstract.Modules (Modules(..))
import Data.Abstract.Declarations as X
import Data.Abstract.Environment as X
import Data.Abstract.Exports as Exports
import Data.Abstract.FreeVariables as X
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Abstract.Package as Package
import Data.ByteString.Char8 (pack, unpack)
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
       => SubtermAlgebra constr term (Evaluator location value effects (ValueRef value))
  default eval :: (Member (Resumable (Unspecialized value)) effects, Show1 constr) => SubtermAlgebra constr term (Evaluator location value effects (ValueRef value))
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
             , Reader Span
             , Resumable (AddressError location value)
             , Resumable (EnvironmentError value)
             , Resumable (EvalError value)
             , Resumable ResolutionError
             , Resumable (Unspecialized value)
             , Return value
             , State (Environment location value)
             , State (Exports location value)
             , State (Heap location (Cell location) value)
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

runEvalError :: Evaluator location value (Resumable (EvalError value) ': effects) a -> Evaluator location value effects (Either (SomeExc (EvalError value)) a)
runEvalError = runResumable

runEvalErrorWith :: (forall resume . EvalError value resume -> Evaluator location value effects resume) -> Evaluator location value (Resumable (EvalError value) ': effects) a -> Evaluator location value effects a
runEvalErrorWith = runResumableWith

-- | Evaluate a term within the context of the scoped environment of 'scopedEnvTerm'.
--   Throws an 'EnvironmentLookupError' if @scopedEnvTerm@ does not have an environment.
evaluateInScopedEnv :: ( AbstractValue location value effects
                       , Members '[ Resumable (EvalError value)
                                  , State (Environment location value)
                                  ] effects
                       )
                    => Evaluator location value effects value
                    -> Evaluator location value effects value
                    -> Evaluator location value effects value
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


throwEvalError :: Member (Resumable (EvalError value)) effects => EvalError value resume -> Evaluator location value effects resume
throwEvalError = throwResumable


data Unspecialized a b where
  Unspecialized :: Prelude.String -> Unspecialized value (ValueRef value)

instance Eq1 (Unspecialized a) where
  liftEq _ (Unspecialized a) (Unspecialized b) = a == b

deriving instance Eq (Unspecialized a b)
deriving instance Show (Unspecialized a b)
instance Show1 (Unspecialized a) where
  liftShowsPrec _ _ = showsPrec

value :: ( Addressable location effects
         , AbstractValue location value effects
         , Members '[ Reader (Environment location value)
                    , Resumable (AddressError location value)
                    , Resumable (EnvironmentError value)
                    , Resumable (EvalError value)
                    , State (Environment location value)
                    , State (Heap location (Cell location) value)
                    ] effects
         )
      => ValueRef value
      -> Evaluator location value effects value
value (LvalLocal var) = variable var
value (LvalMember obj prop) = evaluateInScopedEnv (pure obj) (variable prop)
value (Rval val) = pure val

subtermValue :: ( Addressable location effects
                , AbstractValue location value effects
                , Members '[ Reader (Environment location value)
                           , Resumable (AddressError location value)
                           , Resumable (EnvironmentError value)
                           , Resumable (EvalError value)
                           , State (Environment location value)
                           , State (Heap location (Cell location) value)
                           ] effects
                )
             => Subterm t (ValueRef value)
             -> Evaluator location value effects value
subtermValue = value . subtermRef

runUnspecialized :: Evaluator location value (Resumable (Unspecialized value) ': effects) a -> Evaluator location value effects (Either (SomeExc (Unspecialized value)) a)
runUnspecialized = runResumable

runUnspecializedWith :: (forall resume . Unspecialized value resume -> Evaluator location value effects resume) -> Evaluator location value (Resumable (Unspecialized value) ': effects) a -> Evaluator location value effects a
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
  eval = maybe (Rval <$> unit) (runApp . foldMap1 (App . subtermRef)) . nonEmpty


traceResolve :: (Show a, Show b, Member Trace effects) => a -> b -> Evaluator location value effects ()
traceResolve name path = trace ("resolved " <> show name <> " -> " <> show path)


builtin :: ( Addressable location effects
           , HasCallStack
           , Members '[ Reader (Environment location value)
                      , Reader ModuleInfo
                      , Reader Span
                      , State (Environment location value)
                      , State (Heap location (Cell location) value)
                      ] effects
           , Reducer value (Cell location value)
           )
        => String
        -> Evaluator location value effects value
        -> Evaluator location value effects ()
builtin n def = withCurrentCallStack callStack $ do
  let name = X.name ("__semantic_" <> pack n)
  addr <- alloc name
  modifyEnv (X.insert name addr)
  def >>= assign addr

-- | Evaluate a given package.
evaluatePackageWith :: forall location term value inner inner' outer
                    .  ( Evaluatable (Base term)
                       , EvaluatableConstraints location term value inner
                       , Members '[ Fail
                                  , Fresh
                                  , Reader (Environment location value)
                                  , Resumable (LoadError location value)
                                  , State (Environment location value)
                                  , State (Exports location value)
                                  , State (ModuleTable (Maybe (Environment location value, value)))
                                  , Trace
                                  ] outer
                       , Recursive term
                       , inner ~ (Goto inner' value ': inner')
                       , inner' ~ (LoopControl value ': Return value ': Reader ModuleInfo ': Modules location value ': State (Gotos location value (Reader Span ': Reader PackageInfo ': outer)) ': Reader Span ': Reader PackageInfo ': outer)
                       )
                    => (SubtermAlgebra Module      term (Evaluator location value inner value) -> SubtermAlgebra Module      term (Evaluator location value inner value))
                    -> (SubtermAlgebra (Base term) term (Evaluator location value inner (ValueRef value)) -> SubtermAlgebra (Base term) term (Evaluator location value inner (ValueRef value)))
                    -> Package term
                    -> Evaluator location value outer [value]
evaluatePackageWith analyzeModule analyzeTerm package
  = runReader (packageInfo package)
  . runReader lowerBound
  . fmap fst
  . runState (lowerBound :: Gotos location value (Reader Span ': Reader PackageInfo ': outer))
  . runReader (packageModules (packageBody package))
  . runModules evalModule
  . withPrelude (packagePrelude (packageBody package))
  $ traverse (uncurry evaluateEntryPoint) (ModuleTable.toPairs (packageEntryPoints (packageBody package)))
  where
        evalModule :: Module term
                   -> Evaluator
                        location
                        value
                        (Modules location value
                           : State
                               (Gotos location value (Reader Span : Reader PackageInfo : outer))
                           : Reader Span : Reader PackageInfo : outer)
                        (Environment location value, value)
        evalModule m
          = pairValueWithEnv
          . runInModule (moduleInfo m)
          . analyzeModule (subtermRef . moduleBody)
          -- . fmap (\x -> let () = x in undefined)
          $ evalTerm <$> m
        evalTerm term = Subterm term
          (value =<< foldSubterms (analyzeTerm eval) term)

        runInModule info
          = runReader info
          . runReturn
          . runLoopControl
          . runGoto Gotos getGotos

        evaluateEntryPoint :: ModulePath -> Maybe Name -> Evaluator location value (Modules location value ': State (Gotos location value (Reader Span ': Reader PackageInfo ': outer)) ': Reader Span ': Reader PackageInfo ': outer) value
        evaluateEntryPoint m sym = runInModule (ModuleInfo m) $ do
          v <- maybe unit (pure . snd) <$> require m
          maybe v ((`call` []) <=< variable) sym

        withPrelude Nothing a = a
        withPrelude (Just prelude) a = do
          _ <- runInModule moduleInfoFromCallStack $ do
            builtin "print" (closure ["s"] lowerBound (variable "s" >>= asString >>= trace . unpack >> unit))
            unit
          preludeEnv <- fst <$> evalModule prelude
          withDefaultEnvironment preludeEnv a

        -- TODO: If the set of exports is empty because no exports have been
        -- defined, do we export all terms, or no terms? This behavior varies across
        -- languages. We need better semantics rather than doing it ad-hoc.
        filterEnv ports env
          | Exports.null ports = env
          | otherwise          = Exports.toEnvironment ports `mergeEnvs` overwrite (Exports.aliases ports) env
        pairValueWithEnv action = flip (,) <$> action <*> (filterEnv <$> getExports <*> getEnv)

newtype Gotos location value outer = Gotos { getGotos :: GotoTable (LoopControl value ': Return value ': Reader ModuleInfo ': Modules location value ': State (Gotos location value outer) ': outer) value }
  deriving (Lower)


-- | Isolate the given action with an empty global environment and exports.
isolate :: Members '[State (Environment location value), State (Exports location value)] effects => Evaluator location value effects a -> Evaluator location value effects a
isolate = withEnv lowerBound . withExports lowerBound
