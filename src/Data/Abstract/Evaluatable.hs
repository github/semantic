{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Abstract.Evaluatable
( module X
, Evaluatable(..)
, traceResolve
-- * Preludes
, HasPrelude(..)
-- * Effects
, EvalError(..)
, throwEvalError
, throwNoNameError
, runEvalError
, runEvalErrorWith
, UnspecializedError(..)
, runUnspecialized
, runUnspecializedWith
, throwUnspecializedError
, __self
) where

import           Control.Algebra
import qualified Control.Carrier.Resumable.Either as Either
import qualified Control.Carrier.Resumable.Resume as With
import           Data.Foldable
import           Data.Functor.Classes
import           Data.List.NonEmpty (nonEmpty)
import           Data.Scientific (Scientific)
import           Data.Semigroup.Foldable
import           Data.Semilattice.Lower
import           Data.Sum
import           Data.Text
import           GHC.Stack
import           Source.Span (HasSpan (..))

import           Analysis.Name as X
import           Control.Abstract hiding (Load, String)
import qualified Control.Abstract as Abstract
import           Control.Abstract.Context as X
import           Control.Abstract.Evaluator as X hiding
    (LoopControl (..), Return (..), catchLoopControl, catchReturn, runLoopControl, runReturn)
import           Control.Abstract.Modules as X
    ( ModuleResult
    , Modules
    , ResolutionError (..)
    , listModulesInDir
    , load
    , lookupModule
    , require
    , resolve
    , throwResolutionError
    )
import           Control.Abstract.Value as X hiding
    ( Array (..)
    , Bitwise (..)
    , Boolean (..)
    , Function (..)
    , Hash (..)
    , Numeric (..)
    , Object (..)
    , String (..)
    , Unit (..)
    , While (..)
    )
import           Data.Abstract.AccessControls.Class as X
import           Data.Abstract.BaseError as X
import           Data.Abstract.Declarations as X
import           Data.Abstract.FreeVariables as X
import           Data.Abstract.Module
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import           Data.Language
import           Data.Semigroup.App
import           Data.Term

-- | The 'Evaluatable' class defines the necessary interface for a term to be evaluated. While a default definition of 'eval' is given, instances with computational content must implement 'eval' to perform their small-step operational semantics.
class (Show1 constr, Foldable constr) => Evaluatable constr where
  eval :: ( AbstractValue term address value m
          , AccessControls term
          , Declarations term
          , FreeVariables term
          , HasSpan term
          , Has (Allocator address) sig m
          , Has (Bitwise value) sig m
          , Has (Boolean value) sig m
          , Has (While value) sig m
          , Has (Deref value) sig m
          , Has (State (ScopeGraph address)) sig m
          , Has (Error (LoopControl value)) sig m
          , Has (Error (Return value)) sig m
          , Has Fresh sig m
          , Has (Function term address value) sig m
          , Has (Modules address value) sig m
          , Has (Numeric value) sig m
          , Has (Object address value) sig m
          , Has (Array value) sig m
          , Has (Hash value) sig m
          , Has (Reader ModuleInfo) sig m
          , Has (Reader PackageInfo) sig m
          , Has (Reader Span) sig m
          , Has (State Span) sig m
          , Has (Abstract.String value) sig m
          , Has (Reader (CurrentFrame address)) sig m
          , Has (Reader (CurrentScope address)) sig m
          , Has (Resumable (BaseError (ScopeError address))) sig m
          , Has (Resumable (BaseError (HeapError address))) sig m
          , Has (Resumable (BaseError (AddressError address value))) sig m
          , Has (Resumable (BaseError (UnspecializedError address value))) sig m
          , Has (Resumable (BaseError (EvalError term address value))) sig m
          , Has (Resumable (BaseError ResolutionError)) sig m
          , Has (State (Heap address address value)) sig m
          , Has Trace sig m
          , Has (Unit value) sig m
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
         , Declarations term
         , Has (Object address value) sig m
         , Has (Reader (CurrentFrame address)) sig m
         , Has (Reader (CurrentScope address)) sig m
         , Has (Reader ModuleInfo) sig m
         , Has (Reader Span) sig m
         , Has (Resumable (BaseError (EvalError term address value))) sig m
         , Has (Resumable (BaseError (HeapError address))) sig m
         , Has (Resumable (BaseError (ScopeError address))) sig m
         , Has (Resumable (BaseError (UnspecializedError address value))) sig m
         , Has (State (Heap address address value)) sig m
         , Has (State (ScopeGraph address)) sig m
         , Ord address
         )
      => (term -> Evaluator term address value m value)
      -> (term -> Evaluator term address value m (Slot address))
      -> (constr term -> Evaluator term address value m (Slot address))
  ref _ _ expr = do
    throwUnspecializedError $ RefUnspecializedError ("ref unspecialized for " <> liftShowsPrec (const (const id)) (const id) 0 expr "")


traceResolve :: (Show a, Show b, Has Trace sig m) => a -> b -> Evaluator term address value m ()
traceResolve name path = trace ("resolved " <> show name <> " -> " <> show path)

__self :: Name
__self = name "__semantic_self"

-- Preludes

class HasPrelude (language :: Language) where
  definePrelude :: ( AbstractValue term address value m
                   , HasCallStack
                   , Has (Allocator address) sig m
                   , Has (State (ScopeGraph address)) sig m
                   , Has (Resumable (BaseError (ScopeError address))) sig m
                   , Has (Resumable (BaseError (HeapError address))) sig m
                   , Has (Deref value) sig m
                   , Has Fresh sig m
                   , Has (Function term address value) sig m
                   , Has (Reader ModuleInfo) sig m
                   , Has (Reader Span) sig m
                   , Has (Resumable (BaseError (AddressError address value))) sig m
                   , Has (State (Heap address address value)) sig m
                   , Has (Reader (CurrentFrame address)) sig m
                   , Has (Reader (CurrentScope address)) sig m
                   , Has Trace sig m
                   , Has (Unit value) sig m
                   , Has (Object address value) sig m
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
    defineBuiltIn (Declaration $ X.name "print") Default Public Print

instance HasPrelude 'Ruby where
  definePrelude _ = do
    defineSelf

    defineBuiltIn (Declaration $ X.name "puts") Default Public Print

    defineClass (Declaration (X.name "Object")) [] $ do
      defineBuiltIn (Declaration $ X.name "inspect") Default Public Show

instance HasPrelude 'TSX

instance HasPrelude 'TypeScript where
  definePrelude _ = do
    defineSelf
    defineNamespace (Declaration (X.name "console")) $ defineBuiltIn (Declaration $ X.name "log") Default Public Print

instance HasPrelude 'JavaScript where
  definePrelude _ = do
    defineSelf
    defineNamespace (Declaration (X.name "console")) $ defineBuiltIn (Declaration $ X.name "log") Default Public Print

defineSelf :: ( Has (State (ScopeGraph address)) sig m
              , Has (Resumable (BaseError (ScopeError address))) sig m
              , Has (Resumable (BaseError (HeapError address))) sig m
              , Has (Deref value) sig m
              , Has (Reader ModuleInfo) sig m
              , Has (Reader Span) sig m
              , Has (State (Heap address address value)) sig m
              , Has (Reader (CurrentFrame address)) sig m
              , Has (Reader (CurrentScope address)) sig m
              , Has (Object address value) sig m
              , Ord address
              )
           => Evaluator term address value m ()
defineSelf = do
  let self = Declaration __self
  declare self ScopeGraph.Prelude Public lowerBound ScopeGraph.Unknown Nothing
  slot <- lookupSlot self
  assign slot =<< object =<< currentFrame


-- Effects

-- | The type of error thrown when failing to evaluate a term.
data EvalError term address value return where
  AccessControlError  :: (Name, AccessControl) -> (Name, AccessControl) -> value -> EvalError term address value value
  ConstructorError    :: Name -> EvalError term address value address
  DefaultExportError  :: EvalError term address value ()
  DerefError          :: value -> EvalError term address value value
  ExportError         :: ModulePath -> Name -> EvalError term address value ()
  FloatFormatError    :: Text -> EvalError term address value Scientific
  -- Indicates that our evaluator wasn't able to make sense of these literals.
  IntegerFormatError  :: Text -> EvalError term address value Integer
  NoNameError         :: term -> EvalError term address value Name
  RationalFormatError :: Text -> EvalError term address value Rational
  ReferenceError      :: value -> term -> EvalError term address value (Slot address)
  ScopedEnvError      :: value -> EvalError term address value address

throwNoNameError :: ( Has (Reader ModuleInfo) sig m
                    , Has (Reader Span) sig m
                    , Has (Resumable (BaseError (EvalError term address value))) sig m
                    )
                => term
                -> Evaluator term address value m Name
throwNoNameError = throwEvalError . NoNameError

deriving instance (Eq term, Eq value) => Eq (EvalError term address value return)
deriving instance (Show term, Show value) => Show (EvalError term address value return)

instance (Eq term, Eq value) => Eq1 (EvalError term address value) where
  liftEq _ (AccessControlError a b c) (AccessControlError a' b' c') = a == a' && b == b' && c == c'
  liftEq _ (DerefError v) (DerefError v2)                           = v == v2
  liftEq _ DefaultExportError DefaultExportError                    = True
  liftEq _ (ExportError a b) (ExportError c d)                      = a == c && b == d
  liftEq _ (FloatFormatError a) (FloatFormatError b)                = a == b
  liftEq _ (IntegerFormatError a) (IntegerFormatError b)            = a == b
  liftEq _ (NoNameError t1)       (NoNameError t2)                  = t1 == t2
  liftEq _ (RationalFormatError a) (RationalFormatError b)          = a == b
  liftEq _ (ReferenceError v n) (ReferenceError v2 n2)              = v == v2 && n == n2
  liftEq _ _ _                                                      = False

instance (Show term, Show value) => Show1 (EvalError term address value) where
  liftShowsPrec _ _ = showsPrec

runEvalError :: Evaluator term address value (Either.ResumableC (BaseError (EvalError term address value)) m) a
             -> Evaluator term address value m (Either (Either.SomeError (BaseError (EvalError term address value))) a)
runEvalError = raiseHandler Either.runResumable

runEvalErrorWith :: (forall resume . (BaseError (EvalError term address value)) resume -> Evaluator term address value m resume)
                 -> Evaluator term address value (With.ResumableC (BaseError (EvalError term address value)) m) a
                 -> Evaluator term address value m a
runEvalErrorWith f = raiseHandler $ With.runResumable (runEvaluator . f)

throwEvalError :: ( Has (Reader ModuleInfo) sig m
                  , Has (Reader Span) sig m
                  , Has (Resumable (BaseError (EvalError term address value))) sig m
                  )
               => EvalError term address value resume
               -> Evaluator term address value m resume
throwEvalError = throwBaseError


data UnspecializedError address value resume where
  UnspecializedError    :: String -> UnspecializedError address value value
  RefUnspecializedError :: String -> UnspecializedError address value (Slot address)

deriving instance Eq   (UnspecializedError address value resume)
deriving instance Show (UnspecializedError address value resume)


instance Eq1 (UnspecializedError address value) where
  liftEq _ (UnspecializedError a)    (UnspecializedError b)    = a == b
  liftEq _ (RefUnspecializedError a) (RefUnspecializedError b) = a == b
  liftEq _ _                         _                         = False

instance Show1 (UnspecializedError address value) where
  liftShowsPrec _ _ = showsPrec

runUnspecialized :: Evaluator term address value (Either.ResumableC (BaseError (UnspecializedError address value)) m) a
                 -> Evaluator term address value m (Either (Either.SomeError (BaseError (UnspecializedError address value))) a)
runUnspecialized = raiseHandler Either.runResumable

runUnspecializedWith :: (forall resume . BaseError (UnspecializedError address value) resume -> Evaluator term address value m resume)
                     -> Evaluator term address value (With.ResumableC (BaseError (UnspecializedError address value)) m) a
                     -> Evaluator term address value m a
runUnspecializedWith f = raiseHandler $ With.runResumable (runEvaluator . f)


throwUnspecializedError :: ( Has (Resumable (BaseError (UnspecializedError address value))) sig m
                           , Has (Reader ModuleInfo) sig m
                           , Has (Reader Span) sig m
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
  eval eval _ = maybe unit (runApp . foldMap1 (App . eval)) . nonEmpty
