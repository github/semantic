{-# LANGUAGE GADTs, KindSignatures, RankNTypes, TypeOperators, UndecidableInstances, ScopedTypeVariables, InstanceSigs, ScopedTypeVariables #-}
module Data.Abstract.Evaluatable
( module X
, Evaluatable(..)
, traceResolve
-- * Preludes
, HasPrelude(..)
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
import Control.Abstract.Evaluator as X hiding (LoopControl(..), Return(..), catchLoopControl, runLoopControl, catchReturn, runReturn)
import Control.Abstract.Modules as X (Modules, ModuleResult, ResolutionError(..), load, lookupModule, listModulesInDir, require, resolve, throwResolutionError)
import Control.Abstract.Value as X hiding (Boolean(..), Function(..), While(..))
import Data.Abstract.Declarations as X
import Data.Abstract.BaseError as X
import Data.Abstract.FreeVariables as X
import Data.Abstract.Module
import Data.Abstract.Name as X
import Data.Abstract.Ref as X
import Data.Language
import Data.Scientific (Scientific)
import Data.Semigroup.App
import Data.Semigroup.Foldable
import Data.Sum hiding (project)
import Data.Term
import Prologue
import Data.ImportPath (ImportPath)

-- | The 'Evaluatable' class defines the necessary interface for a term to be evaluated. While a default definition of 'eval' is given, instances with computational content must implement 'eval' to perform their small-step operational semantics.
class (Show1 constr, Foldable constr) => Evaluatable constr where
  eval :: ( AbstractValue term address value m
          , Carrier sig m
          , Declarations term
          , FreeVariables term
          , Member (Allocator address) sig
          , Member (Boolean value) sig
          , Member (While address value) sig
          , Member (Deref value) sig
          , Member (State (ScopeGraph address)) sig
          , Member (Error (LoopControl address value)) sig
          , Member (Error (Return address value)) sig
          , Member Fresh sig
          , Member (Function term address value) sig
          , Member (Modules address value) sig
          , Member (Reader ModuleInfo) sig
          , Member (Reader PackageInfo) sig
          , Member (Reader Span) sig
          , Member (State Span) sig
          , Member (Reader (address, address)) sig
          , Member (Resumable (BaseError (ScopeError address))) sig
          , Member (Resumable (BaseError (HeapError address))) sig
          , Member (Resumable (BaseError (AddressError address value))) sig
          , Member (Resumable (BaseError (UnspecializedError value))) sig
          , Member (Resumable (BaseError (EvalError address value))) sig
          , Member (Resumable (BaseError ResolutionError)) sig
          , Member (State (Heap address address value)) sig
          , Member Trace sig
          , Ord address
          , Show address
          )
       => (term -> Evaluator term address value m (ValueRef address value))
       -> (constr term -> Evaluator term address value m (ValueRef address value))
  eval recur expr = do
    traverse_ recur expr
    v <- throwUnspecializedError $ UnspecializedError ("Eval unspecialized for " <> liftShowsPrec (const (const id)) (const id) 0 expr "")
    rvalBox v


traceResolve :: (Show a, Show b, Member Trace sig, Carrier sig m) => a -> b -> Evaluator term address value m ()
traceResolve name path = trace ("resolved " <> show name <> " -> " <> show path)


-- Preludes

class HasPrelude (language :: Language) where
  definePrelude :: ( AbstractValue term address value m
                   , Carrier sig m
                   , HasCallStack
                   , Member (Allocator address) sig
                   , Member (State (ScopeGraph address)) sig
                   , Member (Resumable (BaseError (ScopeError address))) sig
                   , Member (Resumable (BaseError (HeapError address))) sig
                   , Member (Deref value) sig
                   , Member Fresh sig
                   , Member (Function term address value) sig
                   , Member (Reader ModuleInfo) sig
                   , Member (Reader Span) sig
                   , Member (Resumable (BaseError (AddressError address value))) sig
                   , Member (State (Heap address address value)) sig
                   , Member (Reader (address, address)) sig
                   , Member Trace sig
                   , Ord address
                   , Show address
                   )
                => proxy language
                -> Evaluator term address value m ()
  definePrelude _ = pure ()

instance HasPrelude 'Go
instance HasPrelude 'Haskell
instance HasPrelude 'Java
instance HasPrelude 'PHP

instance HasPrelude 'Python where
  definePrelude _ =
    void . withLexicalScopeAndFrame $
      define (Declaration $ X.name "print") (builtIn (X.name "print") Print)

instance HasPrelude 'Ruby where
  definePrelude _ = do
    void $ builtIn (X.name "puts") Print

    defineClass (Declaration (X.name "Object")) [] $ do
      void $ builtIn (X.name "inspect") Show

instance HasPrelude 'TypeScript where
  definePrelude _ = pure ()
    -- defineNamespace (Declaration (X.name "console")) (builtIn (X.name "log") Print)

instance HasPrelude 'JavaScript where
  definePrelude _ = do
    defineNamespace (Declaration (X.name "console")) $ builtIn (X.name "log") Print


-- Effects

-- | The type of error thrown when failing to evaluate a term.
data EvalError address value return where
  QualifiedImportError :: ImportPath -> EvalError address value (ValueRef address value)
  DerefError :: value -> EvalError address value (ValueRef address value)
  DefaultExportError  :: EvalError address value ()
  ExportError         :: ModulePath -> Name -> EvalError address value ()
  -- Indicates that our evaluator wasn't able to make sense of these literals.
  FloatFormatError    :: Text -> EvalError address value Scientific
  IntegerFormatError  :: Text -> EvalError address value Integer
  NoNameError :: EvalError address value Name
  RationalFormatError :: Text -> EvalError address value Rational
  ReferenceError      :: value -> Name -> EvalError address value (ValueRef address value)

deriving instance (Eq address, Eq value) => Eq (EvalError address value return)
deriving instance (Show address, Show value) => Show (EvalError address value return)

instance NFData value => NFData1 (EvalError address value) where
  liftRnf _ x = case x of
    DerefError v -> rnf v
    DefaultExportError -> ()
    ExportError p n -> rnf p `seq` rnf n
    FloatFormatError i -> rnf i
    IntegerFormatError i -> rnf i
    NoNameError -> ()
    RationalFormatError i -> rnf i
    ReferenceError v n -> rnf v `seq` rnf n
    QualifiedImportError i -> rnf i

instance (NFData value, NFData return) => NFData (EvalError address value return) where
  rnf = liftRnf rnf

instance Eq value => Eq1 (EvalError address value) where
  liftEq _ (DerefError v) (DerefError v2) = v == v2
  liftEq _ DefaultExportError DefaultExportError           = True
  liftEq _ (ExportError a b) (ExportError c d)             = (a == c) && (b == d)
  liftEq _ (FloatFormatError a) (FloatFormatError b)       = a == b
  liftEq _ (IntegerFormatError a) (IntegerFormatError b)   = a == b
  liftEq _ NoNameError        NoNameError                  = True
  liftEq _ (RationalFormatError a) (RationalFormatError b) = a == b
  liftEq _ (ReferenceError v n) (ReferenceError v2 n2)     = (v == v2) && (n == n2)
  liftEq _ _ _                                             = False

instance (Show address, Show value) => Show1 (EvalError address value) where
  liftShowsPrec _ _ = showsPrec

runEvalError :: (Carrier sig m, Effect sig) => Evaluator term address value (ResumableC (BaseError (EvalError address value)) (Eff m)) a -> Evaluator term address value m (Either (SomeError (BaseError (EvalError address value))) a)
runEvalError = raiseHandler runResumable

runEvalErrorWith :: Carrier sig m => (forall resume . (BaseError (EvalError address value)) resume -> Evaluator term address value m resume) -> Evaluator term address value (ResumableWithC (BaseError (EvalError address value)) (Eff m)) a -> Evaluator term address value m a
runEvalErrorWith f = raiseHandler $ runResumableWith (runEvaluator . f)

throwEvalError :: ( Member (Reader ModuleInfo) sig
                  , Member (Reader Span) sig
                  , Member (Resumable (BaseError (EvalError address value))) sig
                  , Carrier sig m
                  )
               => EvalError address value resume
               -> Evaluator term address value m resume
throwEvalError = throwBaseError


data UnspecializedError a b where
  UnspecializedError :: String -> UnspecializedError value value

instance NFData1 (UnspecializedError a) where
  liftRnf _ (UnspecializedError s) = rnf s

instance NFData b => NFData (UnspecializedError a b) where
  rnf = liftRnf rnf

deriving instance Eq (UnspecializedError a b)
deriving instance Show (UnspecializedError a b)


instance Eq1 (UnspecializedError a) where
  liftEq _ (UnspecializedError a) (UnspecializedError b) = a == b

instance Show1 (UnspecializedError a) where
  liftShowsPrec _ _ = showsPrec

runUnspecialized :: (Carrier sig m, Effect sig)
                 => Evaluator term address value (ResumableC (BaseError (UnspecializedError value)) (Eff m)) a
                 -> Evaluator term address value m (Either (SomeError (BaseError (UnspecializedError value))) a)
runUnspecialized = raiseHandler runResumable

runUnspecializedWith :: Carrier sig m
                     => (forall resume . BaseError (UnspecializedError value) resume -> Evaluator term address value m resume)
                     -> Evaluator term address value (ResumableWithC (BaseError (UnspecializedError value)) (Eff m)) a
                     -> Evaluator term address value m a
runUnspecializedWith f = raiseHandler $ runResumableWith (runEvaluator . f)


throwUnspecializedError :: ( Member (Resumable (BaseError (UnspecializedError value))) sig
                           , Member (Reader ModuleInfo) sig
                           , Member (Reader Span) sig
                           , Carrier sig m
                           )
                        => UnspecializedError value resume
                        -> Evaluator term address value m resume
throwUnspecializedError = throwBaseError


-- Instances

-- | If we can evaluate any syntax which can occur in a 'Sum', we can evaluate the 'Sum'.
instance (Apply Evaluatable fs, Apply Show1 fs, Apply Foldable fs) => Evaluatable (Sum fs) where
  eval eval' = apply @Evaluatable (eval eval')

-- | Evaluating a 'TermF' ignores its annotation, evaluating the underlying syntax.
instance (Evaluatable s, Show a) => Evaluatable (TermF s a) where
  eval eval' = eval eval' . termFOut


-- NOTE: Use 'Data.Syntax.Statements' instead of '[]' if you need imperative eval semantics.
--
-- | '[]' is treated as an imperative sequence of statements/declarations s.t.:
--
--   1. Each statement’s effects on the store are accumulated;
--   2. Each statement can affect the environment of later statements (e.g. by 'modify'-ing the environment); and
--   3. Only the last statement’s return value is returned.
instance Evaluatable [] where
  -- 'nonEmpty' and 'foldMap1' enable us to return the last statement’s result instead of 'unit' for non-empty lists.
  eval eval = maybe (rvalBox unit) (runApp . foldMap1 (App . eval)) . nonEmpty
