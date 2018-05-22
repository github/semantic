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
, rvalBox
, value
, address
, subtermValue
, subtermAddress
, evaluateInScopedEnv
, evaluatePackageWith
, throwEvalError
, traceResolve
, builtin
, isolate
, Modules
) where

import Control.Abstract as X hiding (Goto(..), LoopControl(..), Modules(..), Return(..), TermEvaluator(..))
import Control.Abstract.Evaluator (LoopControl, Return(..))
import Control.Abstract.Goto (Goto(..))
import Control.Abstract.Modules (Modules(..))
import Control.Abstract.TermEvaluator (TermEvaluator(..))
import Data.Abstract.Address
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
       => SubtermAlgebra constr term (Evaluator location value effects (ValueRef location value))
  default eval :: ( Members '[ Resumable (Unspecialized value)
                             , Allocator location value
                             , State (Heap location (Cell location) value)
                             ] effects
                  , Show1 constr
                  , Ord location
                  , Reducer value (Cell location value)
                  )
               => SubtermAlgebra constr term (Evaluator location value effects (ValueRef location value))
  eval expr = rvalBox =<< throwResumable (Unspecialized ("Eval unspecialized for " ++ liftShowsPrec (const (const id)) (const id) 0 expr ""))

type EvaluatableConstraints location term value effects =
  ( AbstractValue location value effects
  , Declarations term
  , FreeVariables term
  , Members '[ Allocator location value
             , LoopControl location value
             , Modules location value
             , Reader (Environment location value)
             , Reader ModuleInfo
             , Reader PackageInfo
             , Reader Span
             , Resumable (EnvironmentError value)
             , Resumable (EvalError value)
             , Resumable ResolutionError
             , Resumable (Unspecialized value)
             , Return location value
             , State (Environment location value)
             , State (Exports location value)
             , State (Heap location (Cell location) value)
             , Trace
             ] effects
  , Ord location
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

runEvalError :: Effectful (m value) => m value (Resumable (EvalError value) ': effects) a -> m value effects (Either (SomeExc (EvalError value)) a)
runEvalError = runResumable

runEvalErrorWith :: Effectful (m value) => (forall resume . EvalError value resume -> m value effects resume) -> m value (Resumable (EvalError value) ': effects) a -> m value effects a
runEvalErrorWith = runResumableWith

-- | Evaluate a term within the context of the scoped environment of 'scopedEnvTerm'.
--   Throws an 'EnvironmentLookupError' if @scopedEnvTerm@ does not have an environment.
evaluateInScopedEnv :: ( AbstractValue location value effects
                       , Members '[ Resumable (EvalError value)
                                  , Allocator location value
                                  , State (Environment location value)
                                  , State (Heap location (Cell location) value)
                                  ] effects
                       , Ord location
                       , Reducer value (Cell location value)
                       )
                    => Evaluator location value effects value
                    -> Evaluator location value effects (Address location value)
                    -> Evaluator location value effects (Address location value)
evaluateInScopedEnv scopedEnvTerm term = do
  value <- scopedEnvTerm
  scopedEnv <- scopedEnvironment value
  let mab x = localEnv (mergeEnvs x) term
  maybe (box =<< throwEvalError (EnvironmentLookupError value)) mab scopedEnv

deriving instance Eq value => Eq (EvalError value resume)
deriving instance Show value => Show (EvalError value resume)
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


data Unspecialized value b where
  Unspecialized :: Prelude.String -> Unspecialized value value

instance Eq1 (Unspecialized value) where
  liftEq _ (Unspecialized a) (Unspecialized b) = a == b

deriving instance Eq (Unspecialized value resume)
deriving instance Show (Unspecialized value resume)
instance Show1 (Unspecialized value) where
  liftShowsPrec _ _ = showsPrec

-- | Evaluates a 'Value' returning the referenced value
value :: ( AbstractValue location value effects
         , Members '[ Allocator location value
                    , Reader (Environment location value)
                    , Resumable (EnvironmentError value)
                    , Resumable (EvalError value)
                    , State (Environment location value)
                    , State (Heap location (Cell location) value)
                    ] effects
         , Ord location
         , Reducer value (Cell location value)
         )
      => ValueRef location value
      -> Evaluator location value effects value
value (LvalLocal var) = variable var
value (LvalMember obj prop) = deref =<< evaluateInScopedEnv (deref obj) (fromJust <$> lookupEnv prop)
value (Rval val) = deref val

address :: ( AbstractValue location value effects
           , Members '[ Allocator location value
                      , Reader (Environment location value)
                      , Resumable (EnvironmentError value)
                      , Resumable (EvalError value)
                      , State (Environment location value)
                      , State (Heap location (Cell location) value)
                      ] effects
           , Ord location
           , Reducer value (Cell location value)
           )
        => ValueRef location value
        -> Evaluator location value effects (Address location value)
address (LvalLocal var) = fromJust <$> lookupEnv var
address (LvalMember obj prop) = evaluateInScopedEnv (deref obj) (fromJust <$> lookupEnv prop)
address (Rval addr) = pure addr

rvalBox :: ( Members '[ Allocator location value
                      , State (Heap location (Cell location) value)
                      ] effects
           , Ord location
           , Reducer value (Cell location value)
           )
        => value
        -> Evaluator location value effects (ValueRef location value)
rvalBox val = Rval <$> (box val)

-- | Evaluates a 'Subterm' to its rval
subtermValue :: ( AbstractValue location value effects
                , Members '[ Allocator location value
                           , Reader (Environment location value)
                           , Resumable (EnvironmentError value)
                           , Resumable (EvalError value)
                           , State (Environment location value)
                           , State (Heap location (Cell location) value)
                           ] effects
                , Ord location
                , Reducer value (Cell location value)
                )
             => Subterm term (Evaluator location value effects (ValueRef location value))
             -> Evaluator location value effects value
subtermValue = value <=< subtermRef

subtermAddress :: ( AbstractValue location value effects
                , Members '[ Allocator location value
                           , Reader (Environment location value)
                           , Resumable (EnvironmentError value)
                           , Resumable (EvalError value)
                           , State (Environment location value)
                           , State (Heap location (Cell location) value)
                           ] effects
                , Ord location
                , Reducer value (Cell location value)
                )
             => Subterm term (Evaluator location value effects (ValueRef location value))
             -> Evaluator location value effects (Address location value)
subtermAddress = address <=< subtermRef

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

--- | '[]' is treated as an imperative sequence of statements/declarations s.t.:
---
---   1. Each statement’s effects on the store are accumulated;
---   2. Each statement can affect the environment of later statements (e.g. by 'modify'-ing the environment); and
---   3. Only the last statement’s return value is returned.
instance Evaluatable [] where
  -- 'nonEmpty' and 'foldMap1' enable us to return the last statement’s result instead of 'unit' for non-empty lists.
  eval = maybe (rvalBox =<< unit) (runApp . foldMap1 (App . subtermRef)) . nonEmpty


traceResolve :: (Show a, Show b, Member Trace effects) => a -> b -> Evaluator location value effects ()
traceResolve name path = trace ("resolved " <> show name <> " -> " <> show path)


builtin :: ( HasCallStack
           , Members '[ Allocator location value
                      , Reader (Environment location value)
                      , Reader ModuleInfo
                      , Reader Span
                      , State (Environment location value)
                      , State (Heap location (Cell location) value)
                      ] effects
           , Ord location
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
                    -- FIXME: It’d be nice if we didn’t have to mention 'Addressable' here at all, but 'Located' locations require knowledge of 'currentModule' to run. Can we fix that? If not, can we factor this effect list out?
                    .  ( Addressable location (Reader ModuleInfo ': Modules location value ': State (Gotos location value (Reader Span ': Reader PackageInfo ': outer)) ': Reader Span ': Reader PackageInfo ': outer)
                       , Evaluatable (Base term)
                       , EvaluatableConstraints location term value inner
                       , Members '[ Fail
                                  , Fresh
                                  , Reader (Environment location value)
                                  , Resumable (AddressError location value)
                                  , Resumable (LoadError location value)
                                  , State (Environment location value)
                                  , State (Exports location value)
                                  , State (Heap location (Cell location) value)
                                  , State (ModuleTable (Maybe (Environment location value, Address location value)))
                                  , Trace
                                  ] outer
                       , Recursive term
                       , inner ~ (Goto inner' location value ': inner')
                       , inner' ~ (LoopControl location value ': Return location value ': Allocator location value ': Reader ModuleInfo ': Modules location value ': State (Gotos location value (Reader Span ': Reader PackageInfo ': outer)) ': Reader Span ': Reader PackageInfo ': outer)
                       )
                    => (SubtermAlgebra Module      term (TermEvaluator term location value inner (Address location value)) -> SubtermAlgebra Module      term (TermEvaluator term location value inner (Address location value)))
                    -> (SubtermAlgebra (Base term) term (TermEvaluator term location value inner (ValueRef location value)) -> SubtermAlgebra (Base term) term (TermEvaluator term location value inner (ValueRef location value)))
                    -> Package term
                    -> TermEvaluator term location value outer [(Address location value)]
evaluatePackageWith analyzeModule analyzeTerm package
  = runReader (packageInfo package)
  . runReader lowerBound
  . fmap fst
  . runState (lowerBound :: Gotos location value (Reader Span ': Reader PackageInfo ': outer))
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
        evalTerm term = Subterm term (TermEvaluator (address =<< runTermEvaluator (foldSubterms (analyzeTerm (TermEvaluator . eval . fmap (second runTermEvaluator))) term)))

        runInModule info val
          = runReader info
          $ raiseHandler runAllocator
          $ raiseHandler runReturn
          $ raiseHandler runLoopControl
          $ raiseHandler (runGoto Gotos getGotos)
          $ val

        evaluateEntryPoint :: ModulePath
                           -> Maybe Name
                           -> TermEvaluator term location value (Modules location value ': State (Gotos location value (Reader Span ': Reader PackageInfo ': outer)) ': Reader Span ': Reader PackageInfo ': outer) (Address location value)
        evaluateEntryPoint m sym = runInModule (ModuleInfo m) . TermEvaluator $ do
          v <- maybe (box =<< unit) (pure . snd) <$> require m
          maybe v ((`call` []) <=< variable) sym

        evalPrelude prelude = raiseHandler (runModules (runTermEvaluator . evalModule)) $ do
          _ <- runInModule moduleInfoFromCallStack . TermEvaluator $ do
            builtin "print" (closure ["s"] lowerBound (variable "s" >>= asString >>= trace . unpack >> unit))
            box =<< unit
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

        pairValueWithEnv :: pairEffects ~ (Modules location value ': State (Gotos location value (Reader Span ': Reader PackageInfo ': outer)) ': Reader Span ': Reader PackageInfo ': outer)
                         => TermEvaluator term location value pairEffects (Address location value)
                         -> TermEvaluator term location value pairEffects (Environment location value, Address location value)
        pairValueWithEnv action = do
          act <- action
          env <- (filterEnv <$> TermEvaluator getExports <*> TermEvaluator getEnv)
          pure (env, act)

newtype Gotos location value outer = Gotos {
  getGotos :: GotoTable ( LoopControl location value
                          ': Return location value
                          ': Allocator location value
                          ': Reader ModuleInfo
                          ': Modules location value
                          ': State (Gotos location value outer)
                          ': outer)
                        location value }
  deriving (Lower)


-- | Isolate the given action with an empty global environment and exports.
isolate :: Members '[State (Environment location value), State (Exports location value)] effects => Evaluator location value effects a -> Evaluator location value effects a
isolate = withEnv lowerBound . withExports lowerBound
