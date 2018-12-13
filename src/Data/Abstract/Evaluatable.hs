{-# LANGUAGE GADTs, KindSignatures, RankNTypes, TypeOperators, UndecidableInstances, InstanceSigs #-}
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
import Data.Abstract.BaseError as X
import Data.Abstract.Declarations as X
import Data.Abstract.FreeVariables as X
import Data.Abstract.Module
import Data.Abstract.Name as X
import Data.Abstract.ScopeGraph (Relation(..))
import Data.Language
import Data.Scientific (Scientific)
import Data.Semigroup.App
import Data.Semigroup.Foldable
import Data.Span (emptySpan)
import Data.Sum hiding (project)
import Data.Term
import Prologue

-- | The 'Evaluatable' class defines the necessary interface for a term to be evaluated. While a default definition of 'eval' is given, instances with computational content must implement 'eval' to perform their small-step operational semantics.
class (Show1 constr, Foldable constr) => Evaluatable constr where
  eval :: ( AbstractValue term address value m
          , Carrier sig m
          , Declarations term
          , FreeVariables term
          , Member (Allocator address) sig
          , Member (Boolean value) sig
          , Member (While value) sig
          , Member (Deref value) sig
          , Member (State (ScopeGraph address)) sig
          , Member (Error (LoopControl value)) sig
          , Member (Error (Return value)) sig
          , Member Fresh sig
          , Member (Function term address value) sig
          , Member (Modules address value) sig
          , Member (Reader ModuleInfo) sig
          , Member (Reader PackageInfo) sig
          , Member (Reader Span) sig
          , Member (State Span) sig
          , Member (Reader (CurrentFrame address)) sig
          , Member (Reader (CurrentScope address)) sig
          , Member (Resumable (BaseError (ScopeError address))) sig
          , Member (Resumable (BaseError (HeapError address))) sig
          , Member (Resumable (BaseError (AddressError address value))) sig
          , Member (Resumable (BaseError (UnspecializedError address value))) sig
          , Member (Resumable (BaseError (EvalError term address value))) sig
          , Member (Resumable (BaseError ResolutionError)) sig
          , Member (State (Heap address address value)) sig
          , Member Trace sig
          , Ord address
          , Show address
          )
       => (term -> Evaluator term address value m value)
       -> (term -> Evaluator term address value m (Slot address))
       -> (constr term -> Evaluator term address value m value)
  eval recur _ expr = do
    traverse_ recur expr
    throwUnspecializedError $ UnspecializedError ("Eval unspecialized for " <> liftShowsPrec (const (const id)) (const id) 0 expr "")

  ref :: ( AbstractValue term address value m
         , Carrier sig m
         , Declarations term
         , Member (Reader (CurrentFrame address)) sig
         , Member (Reader (CurrentScope address)) sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (EvalError term address value))) sig
         , Member (Resumable (BaseError (HeapError address))) sig
         , Member (Resumable (BaseError (ScopeError address))) sig
         , Member (Resumable (BaseError (UnspecializedError address value))) sig
         , Member (State (Heap address address value)) sig
         , Member (State (ScopeGraph address)) sig
         , Ord address
         )
      => (term -> Evaluator term address value m value)
      -> (term -> Evaluator term address value m (Slot address))
      -> (constr term -> Evaluator term address value m (Slot address))
  ref _ _ expr = do
    throwUnspecializedError $ RefUnspecializedError ("ref unspecialized for " <> liftShowsPrec (const (const id)) (const id) 0 expr "")


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
                   , Member (Reader (CurrentFrame address)) sig
                   , Member (Reader (CurrentScope address)) sig
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
    defineBuiltIn (Declaration $ X.name "print") Default Print

instance HasPrelude 'Ruby where
  definePrelude _ = do
    defineSelf

    defineBuiltIn (Declaration $ X.name "puts") Default Print

    defineClass (Declaration (X.name "Object")) [] $ do
      defineBuiltIn (Declaration $ X.name "inspect") Default Show

instance HasPrelude 'TypeScript where
  definePrelude _ = do
    defineSelf
    defineNamespace (Declaration (X.name "console")) $ defineBuiltIn (Declaration $ X.name "log") Default Print

instance HasPrelude 'JavaScript where
  definePrelude _ = do
    defineSelf
    defineNamespace (Declaration (X.name "console")) $ defineBuiltIn (Declaration $ X.name "log") Default Print

defineSelf :: ( AbstractValue term address value m
              , Carrier sig m
              , Member (State (ScopeGraph address)) sig
              , Member (Resumable (BaseError (ScopeError address))) sig
              , Member (Resumable (BaseError (HeapError address))) sig
              , Member (Deref value) sig
              , Member (Reader ModuleInfo) sig
              , Member (Reader Span) sig
              , Member (State (Heap address address value)) sig
              , Member (Reader (CurrentFrame address)) sig
              , Member (Reader (CurrentScope address)) sig
              , Ord address
              )
           => Evaluator term address value m ()
defineSelf = do
  let self = Declaration $ X.name "__self"
  -- TODO: Should `self` be given a special Relation?
  declare self Default emptySpan Nothing
  slot <- lookupDeclaration self
  assign slot =<< object =<< currentFrame


-- Effects

-- | The type of error thrown when failing to evaluate a term.
data EvalError term address value return where
  ConstructorError    :: Name -> EvalError term address value address
  DefaultExportError  :: EvalError term address value ()
  DerefError          :: value -> EvalError term address value value
  ExportError         :: ModulePath -> Name -> EvalError term address value ()
  FloatFormatError    :: Text -> EvalError term address value Scientific
  -- ^ Indicates that our evaluator wasn't able to make sense of these literals.
  IntegerFormatError  :: Text -> EvalError term address value Integer
  NoNameError         :: term -> EvalError term address value Name
  RationalFormatError :: Text -> EvalError term address value Rational
  ReferenceError      :: value -> Name -> EvalError term address value (Slot address)
  ScopedEnvError      :: value -> EvalError term address value address

deriving instance (Eq term, Eq value) => Eq (EvalError term address value return)
deriving instance (Show term, Show value) => Show (EvalError term address value return)

instance (NFData term, NFData value) => NFData1 (EvalError term address value) where
  liftRnf _ x = case x of
    ConstructorError n -> rnf n
    DefaultExportError -> ()
    DerefError v -> rnf v
    ExportError p n -> rnf p `seq` rnf n
    FloatFormatError i -> rnf i
    IntegerFormatError i -> rnf i
    NoNameError term -> rnf term
    RationalFormatError i -> rnf i
    ReferenceError v n -> rnf v `seq` rnf n
    ScopedEnvError v -> rnf v

instance (NFData term, NFData value, NFData return) => NFData (EvalError term address value return) where
  rnf = liftRnf rnf

instance (Eq term, Eq value) => Eq1 (EvalError term address value) where
  liftEq _ (DerefError v) (DerefError v2)                  = v == v2
  liftEq _ DefaultExportError DefaultExportError           = True
  liftEq _ (ExportError a b) (ExportError c d)             = (a == c) && (b == d)
  liftEq _ (FloatFormatError a) (FloatFormatError b)       = a == b
  liftEq _ (IntegerFormatError a) (IntegerFormatError b)   = a == b
  liftEq _ (NoNameError t1)       (NoNameError t2)         = t1 == t2
  liftEq _ (RationalFormatError a) (RationalFormatError b) = a == b
  liftEq _ (ReferenceError v n) (ReferenceError v2 n2)     = (v == v2) && (n == n2)
  liftEq _ _ _                                             = False

instance (Show term, Show value) => Show1 (EvalError term address value) where
  liftShowsPrec _ _ = showsPrec

runEvalError :: (Carrier sig m, Effect sig) => Evaluator term address value (ResumableC (BaseError (EvalError term address value)) (Eff m)) a -> Evaluator term address value m (Either (SomeError (BaseError (EvalError term address value))) a)
runEvalError = raiseHandler runResumable

runEvalErrorWith :: Carrier sig m => (forall resume . (BaseError (EvalError term address value)) resume -> Evaluator term address value m resume) -> Evaluator term address value (ResumableWithC (BaseError (EvalError term address value)) (Eff m)) a -> Evaluator term address value m a
runEvalErrorWith f = raiseHandler $ runResumableWith (runEvaluator . f)

throwEvalError :: ( Member (Reader ModuleInfo) sig
                  , Member (Reader Span) sig
                  , Member (Resumable (BaseError (EvalError term address value))) sig
                  , Carrier sig m
                  )
               => EvalError term address value resume
               -> Evaluator term address value m resume
throwEvalError = throwBaseError


data UnspecializedError address value resume where
  UnspecializedError    :: String -> UnspecializedError address value value
  RefUnspecializedError :: String -> UnspecializedError address value (Slot address)

instance NFData1 (UnspecializedError address value) where
  liftRnf _ (UnspecializedError s)    = rnf s
  liftRnf _ (RefUnspecializedError s) = rnf s

instance NFData (UnspecializedError address value resume) where
  rnf = liftRnf (const ())

deriving instance Eq   (UnspecializedError address value resume)
deriving instance Show (UnspecializedError address value resume)


instance Eq1 (UnspecializedError address value) where
  liftEq _ (UnspecializedError a)    (UnspecializedError b)    = a == b
  liftEq _ (RefUnspecializedError a) (RefUnspecializedError b) = a == b
  liftEq _ _                         _                         = False

instance Show1 (UnspecializedError address value) where
  liftShowsPrec _ _ = showsPrec

runUnspecialized :: (Carrier sig m, Effect sig)
                 => Evaluator term address value (ResumableC (BaseError (UnspecializedError address value)) (Eff m)) a
                 -> Evaluator term address value m (Either (SomeError (BaseError (UnspecializedError address value))) a)
runUnspecialized = raiseHandler runResumable

runUnspecializedWith :: Carrier sig m
                     => (forall resume . BaseError (UnspecializedError address value) resume -> Evaluator term address value m resume)
                     -> Evaluator term address value (ResumableWithC (BaseError (UnspecializedError address value)) (Eff m)) a
                     -> Evaluator term address value m a
runUnspecializedWith f = raiseHandler $ runResumableWith (runEvaluator . f)


throwUnspecializedError :: ( Member (Resumable (BaseError (UnspecializedError address value))) sig
                           , Member (Reader ModuleInfo) sig
                           , Member (Reader Span) sig
                           , Carrier sig m
                           )
                        => UnspecializedError address value resume
                        -> Evaluator term address value m resume
throwUnspecializedError = throwBaseError


-- Instances

-- | If we can evaluate any syntax which can occur in a 'Sum', we can evaluate the 'Sum'.
instance (Apply Evaluatable fs, Apply Show1 fs, Apply Foldable fs) => Evaluatable (Sum fs) where
  eval eval' ref = apply @Evaluatable (eval eval' ref)
  ref eval ref' = apply @Evaluatable (ref eval ref')

-- | Evaluating a 'TermF' ignores its annotation, evaluating the underlying syntax.
instance (Evaluatable s, Show a) => Evaluatable (TermF s a) where
  eval eval' ref = eval eval' ref . termFOut
  ref eval ref' = ref eval ref' . termFOut


-- NOTE: Use 'Data.Syntax.Statements' instead of '[]' if you need imperative eval semantics.
--
-- | '[]' is treated as an imperative sequence of statements/declarations s.t.:
--
--   1. Each statement’s effects on the store are accumulated;
--   2. Each statement can affect the environment of later statements (e.g. by 'modify'-ing the environment); and
--   3. Only the last statement’s return value is returned.
instance Evaluatable [] where
  -- 'nonEmpty' and 'foldMap1' enable us to return the last statement’s result instead of 'unit' for non-empty lists.
  eval eval _ = maybe (pure unit) (runApp . foldMap1 (App . eval)) . nonEmpty
