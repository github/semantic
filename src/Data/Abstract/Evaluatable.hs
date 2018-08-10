{-# LANGUAGE GADTs, KindSignatures, RankNTypes, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Evaluatable
( module X
, Evaluatable(..)
, evaluate
, traceResolve
-- * Preludes
, HasPrelude(..)
-- * Postludes
, HasPostlude(..)
-- * Effects
, EvalError(..)
, throwEvalError
, runEvalError
, runEvalErrorWith
, UnspecializedError(..)
, runUnspecialized
, runUnspecializedWith
, throwUnspecializedError
) where

import Control.Abstract hiding (Load)
import Control.Abstract.Context as X
import Control.Abstract.Environment as X hiding (runEnvironmentError, runEnvironmentErrorWith)
import Control.Abstract.Evaluator as X hiding (LoopControl(..), Return(..), catchLoopControl, runLoopControl, catchReturn, runReturn)
import Control.Abstract.Heap as X hiding (runAddressError, runAddressErrorWith)
import Control.Abstract.Modules as X (Modules, ModuleResult, ResolutionError(..), load, lookupModule, listModulesInDir, require, resolve, throwResolutionError)
import Control.Abstract.Value as X hiding (Function(..))
import Data.Abstract.Declarations as X
import Data.Abstract.Environment as X
import Data.Abstract.BaseError as X
import Data.Abstract.FreeVariables as X
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Abstract.Name as X
import Data.Abstract.Ref as X
import Data.Coerce
import Data.Language
import Data.Scientific (Scientific)
import Data.Semigroup.App
import Data.Semigroup.Foldable
import Data.Sum
import Data.Term
import Prologue

-- | The 'Evaluatable' class defines the necessary interface for a term to be evaluated. While a default definition of 'eval' is given, instances with computational content must implement 'eval' to perform their small-step operational semantics.
class (Show1 constr, Foldable constr) => Evaluatable constr where
  eval :: ( AbstractValue address value effects
          , Declarations term
          , FreeVariables term
          , Member (Allocator address) effects
          , Member (Deref address value) effects
          , Member (Env address) effects
          , Member (Exc (LoopControl address)) effects
          , Member (Exc (Return address)) effects
          , Member (Function address value) effects
          , Member (Modules address) effects
          , Member (Reader ModuleInfo) effects
          , Member (Reader PackageInfo) effects
          , Member (Reader Span) effects
          , Member (Resumable (BaseError (EnvironmentError address))) effects
          , Member (Resumable (BaseError (UnspecializedError value))) effects
          , Member (Resumable (BaseError EvalError)) effects
          , Member (Resumable (BaseError ResolutionError)) effects
          , Member Fresh effects
          , Member Trace effects
          )
       => SubtermAlgebra constr term (Evaluator address value effects (ValueRef address))
  eval expr = do
    traverse_ subtermValue expr
    v <- throwUnspecializedError $ UnspecializedError ("Eval unspecialized for " <> liftShowsPrec (const (const id)) (const id) 0 expr "")
    rvalBox v


evaluate :: ( AbstractValue address value valueEffects
            , Allocatable address (Reader ModuleInfo ': effects)
            , Derefable address (Allocator address ': Reader ModuleInfo ': effects)
            , Declarations term
            , Effects effects
            , Evaluatable (Base term)
            , FreeVariables term
            , HasPostlude lang
            , HasPrelude lang
            , Member Fresh effects
            , Member (Modules address) effects
            , Member (Reader (ModuleTable (NonEmpty (Module (ModuleResult address))))) effects
            , Member (Reader PackageInfo) effects
            , Member (Reader Span) effects
            , Member (Resumable (BaseError (AddressError address value))) effects
            , Member (Resumable (BaseError (EnvironmentError address))) effects
            , Member (Resumable (BaseError EvalError)) effects
            , Member (Resumable (BaseError ResolutionError)) effects
            , Member (Resumable (BaseError (UnspecializedError value))) effects
            , Member (State (Heap address value)) effects
            , Member Trace effects
            , Ord value
            , Recursive term
            , ValueRoots address value
            , moduleEffects ~ (Exc (LoopControl address) ': Exc (Return address) ': Env address ': Deref address value ': Allocator address ': Reader ModuleInfo ': effects)
            , valueEffects ~ (Function address value ': moduleEffects)
            )
         => proxy lang
         -> (SubtermAlgebra Module      term (TermEvaluator term address value moduleEffects address)           -> SubtermAlgebra Module      term (TermEvaluator term address value moduleEffects address))
         -> (SubtermAlgebra (Base term) term (TermEvaluator term address value valueEffects (ValueRef address)) -> SubtermAlgebra (Base term) term (TermEvaluator term address value valueEffects (ValueRef address)))
         -> (forall x . Evaluator address value valueEffects x -> Evaluator address value moduleEffects x)
         -> [Module term]
         -> TermEvaluator term address value effects (ModuleTable (NonEmpty (Module (ModuleResult address))))
evaluate lang analyzeModule analyzeTerm runValue modules = do
  (preludeBinds, _) <- TermEvaluator . runInModule lowerBound moduleInfoFromCallStack . runValue $ do
    definePrelude lang
    box unit
  foldr (run preludeBinds) ask modules
  where run preludeBinds m rest = do
          evaluated <- coerce
            (runInModule preludeBinds (moduleInfo m))
            (analyzeModule (subtermRef . moduleBody)
            (evalModuleBody <$> m))
          -- FIXME: this should be some sort of Monoidal insert à la the Heap to accommodate multiple Go files being part of the same module.
          local (ModuleTable.insert (modulePath (moduleInfo m)) ((evaluated <$ m) :| [])) rest

        evalModuleBody term = Subterm term (coerce runValue (do
          result <- foldSubterms (analyzeTerm (TermEvaluator . eval . fmap (second runTermEvaluator))) term >>= TermEvaluator . address
          result <$ TermEvaluator (postlude lang)))

        runInModule preludeBinds info
          = runReader info
          . runAllocator
          . runDeref
          . runEnv (EvalContext Nothing (X.push (newEnv preludeBinds)))
          . runReturn
          . runLoopControl


traceResolve :: (Show a, Show b, Member Trace effects) => a -> b -> Evaluator address value effects ()
traceResolve name path = trace ("resolved " <> show name <> " -> " <> show path)


-- Preludes

class HasPrelude (language :: Language) where
  definePrelude :: ( AbstractValue address value effects
                   , HasCallStack
                   , Member (Allocator address) effects
                   , Member (Deref address value) effects
                   , Member (Env address) effects
                   , Member Fresh effects
                   , Member (Function address value) effects
                   , Member (Reader ModuleInfo) effects
                   , Member (Reader Span) effects
                   , Member (Resumable (BaseError (EnvironmentError address))) effects
                   , Member Trace effects
                   )
                => proxy language
                -> Evaluator address value effects ()
  definePrelude _ = pure ()

instance HasPrelude 'Go
instance HasPrelude 'Haskell
instance HasPrelude 'Java
instance HasPrelude 'PHP

instance HasPrelude 'Python where
  definePrelude _ =
    define (name "print") builtInPrint

instance HasPrelude 'Ruby where
  definePrelude _ = do
    define (name "puts") builtInPrint

    defineClass (name "Object") [] $ do
      define (name "inspect") (lambda (box (string "<object>")))

instance HasPrelude 'TypeScript where
  definePrelude _ =
    defineNamespace (name "console") $ do
      define (name "log") builtInPrint

instance HasPrelude 'JavaScript where
  definePrelude _ = do
    defineNamespace (name "console") $ do
      define (name "log") builtInPrint

-- Postludes

class HasPostlude (language :: Language) where
  postlude :: ( AbstractValue address value effects
              , HasCallStack
              , Member (Allocator address) effects
              , Member (Deref address value) effects
              , Member (Env address) effects
              , Member Fresh effects
              , Member (Reader ModuleInfo) effects
              , Member (Reader Span) effects
              , Member (Resumable (BaseError (EnvironmentError address))) effects
              , Member Trace effects
              )
           => proxy language
           -> Evaluator address value effects ()
  postlude _ = pure ()

instance HasPostlude 'Go
instance HasPostlude 'Haskell
instance HasPostlude 'Java
instance HasPostlude 'PHP
instance HasPostlude 'Python
instance HasPostlude 'Ruby
instance HasPostlude 'TypeScript

instance HasPostlude 'JavaScript where
  postlude _ = trace "JS postlude"


-- Effects

-- | The type of error thrown when failing to evaluate a term.
data EvalError return where
  NoNameError :: EvalError Name
  -- Indicates that our evaluator wasn't able to make sense of these literals.
  IntegerFormatError  :: Text -> EvalError Integer
  FloatFormatError    :: Text -> EvalError Scientific
  RationalFormatError :: Text -> EvalError Rational
  DefaultExportError  :: EvalError ()
  ExportError         :: ModulePath -> Name -> EvalError ()

deriving instance Eq (EvalError return)
deriving instance Show (EvalError return)

instance Eq1 EvalError where
  liftEq _ NoNameError        NoNameError                  = True
  liftEq _ DefaultExportError DefaultExportError           = True
  liftEq _ (ExportError a b) (ExportError c d)             = (a == c) && (b == d)
  liftEq _ (IntegerFormatError a) (IntegerFormatError b)   = a == b
  liftEq _ (FloatFormatError a) (FloatFormatError b)       = a == b
  liftEq _ (RationalFormatError a) (RationalFormatError b) = a == b
  liftEq _ _ _                                             = False

instance Show1 EvalError where
  liftShowsPrec _ _ = showsPrec

runEvalError :: (Effectful m, Effects effects) => m (Resumable (BaseError EvalError) ': effects) a -> m effects (Either (SomeExc (BaseError EvalError)) a)
runEvalError = runResumable

runEvalErrorWith :: (Effectful m, Effects effects) => (forall resume . (BaseError EvalError) resume -> m effects resume) -> m (Resumable (BaseError EvalError) ': effects) a -> m effects a
runEvalErrorWith = runResumableWith

throwEvalError :: ( Member (Reader ModuleInfo) effects
                  , Member (Reader Span) effects
                  , Member (Resumable (BaseError EvalError)) effects
                  )
               => EvalError resume
               -> Evaluator address value effects resume
throwEvalError = throwBaseError


data UnspecializedError a b where
  UnspecializedError :: String -> UnspecializedError value value

deriving instance Eq (UnspecializedError a b)
deriving instance Show (UnspecializedError a b)

instance Eq1 (UnspecializedError a) where
  liftEq _ (UnspecializedError a) (UnspecializedError b) = a == b

instance Show1 (UnspecializedError a) where
  liftShowsPrec _ _ = showsPrec

runUnspecialized :: (Effectful (m value), Effects effects)
                 => m value (Resumable (BaseError (UnspecializedError value)) ': effects) a
                 -> m value effects (Either (SomeExc (BaseError (UnspecializedError value))) a)
runUnspecialized = runResumable

runUnspecializedWith :: (Effectful (m value), Effects effects)
                     => (forall resume . BaseError (UnspecializedError value) resume -> m value effects resume)
                     -> m value (Resumable (BaseError (UnspecializedError value)) ': effects) a
                     -> m value effects a
runUnspecializedWith = runResumableWith

throwUnspecializedError :: ( Member (Resumable (BaseError (UnspecializedError value))) effects
                           , Member (Reader ModuleInfo) effects
                           , Member (Reader Span) effects
                           )
                        => UnspecializedError value resume
                        -> Evaluator address value effects resume
throwUnspecializedError = throwBaseError


-- Instances

-- | If we can evaluate any syntax which can occur in a 'Sum', we can evaluate the 'Sum'.
instance (Apply Evaluatable fs, Apply Show1 fs, Apply Foldable fs) => Evaluatable (Sum fs) where
  eval = apply @Evaluatable eval

-- | Evaluating a 'TermF' ignores its annotation, evaluating the underlying syntax.
instance (Evaluatable s, Show a) => Evaluatable (TermF s a) where
  eval = eval . termFOut


-- NOTE: Use 'Data.Syntax.Statements' instead of '[]' if you need imperative eval semantics.
--
-- | '[]' is treated as an imperative sequence of statements/declarations s.t.:
--
--   1. Each statement’s effects on the store are accumulated;
--   2. Each statement can affect the environment of later statements (e.g. by 'modify'-ing the environment); and
--   3. Only the last statement’s return value is returned.
instance Evaluatable [] where
  -- 'nonEmpty' and 'foldMap1' enable us to return the last statement’s result instead of 'unit' for non-empty lists.
  eval = maybe (rvalBox unit) (runApp . foldMap1 (App . subtermRef)) . nonEmpty
