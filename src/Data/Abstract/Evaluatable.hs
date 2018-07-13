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
, Unspecialized(..)
, runUnspecialized
, runUnspecializedWith
, Cell
) where

import Control.Abstract hiding (Load)
import Control.Abstract.Context as X
import Control.Abstract.Environment as X hiding (runEnvironmentError, runEnvironmentErrorWith)
import Control.Abstract.Evaluator as X hiding (LoopControl(..), Return(..), catchLoopControl, runLoopControl, catchReturn, runReturn)
import Control.Abstract.Heap as X hiding (AddressError(..), runAddressError, runAddressErrorWith)
import Control.Abstract.Modules as X (Modules, ResolutionError(..), load, lookupModule, listModulesInDir, require, resolve)
import Control.Abstract.Value as X
import Data.Abstract.Declarations as X
import Data.Abstract.Environment as X
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
import Data.Semigroup.Reducer hiding (unit)
import Data.Sum
import Data.Term
import Prologue

-- | The 'Evaluatable' class defines the necessary interface for a term to be evaluated. While a default definition of 'eval' is given, instances with computational content must implement 'eval' to perform their small-step operational semantics.
class (Show1 constr, Foldable constr) => Evaluatable constr where
  eval :: ( AbstractValue address value effects
          , Declarations term
          , FreeVariables term
          , Member (Allocator address value) effects
          , Member (Env address) effects
          , Member (Exc (LoopControl address)) effects
          , Member (Exc (Return address)) effects
          , Member Fresh effects
          , Member (Modules address) effects
          , Member (Reader ModuleInfo) effects
          , Member (Reader PackageInfo) effects
          , Member (Reader Span) effects
          , Member (Resumable (EnvironmentError address)) effects
          , Member (Resumable EvalError) effects
          , Member (Resumable ResolutionError) effects
          , Member (Resumable (Unspecialized value)) effects
          , Member Trace effects
          )
       => SubtermAlgebra constr term (Evaluator address value effects (ValueRef address))
  eval expr = do
    traverse_ subtermValue expr
    v <- throwResumable (Unspecialized ("Eval unspecialized for " <> liftShowsPrec (const (const id)) (const id) 0 expr ""))
    rvalBox v


evaluate :: ( AbstractValue address value inner
            , Addressable address (Reader ModuleInfo ': effects)
            , Declarations term
            , Effects effects
            , Evaluatable (Base term)
            , Foldable (Cell address)
            , FreeVariables term
            , HasPostlude lang
            , HasPrelude lang
            , Member Fresh effects
            , Member (Modules address) effects
            , Member (Reader (ModuleTable (NonEmpty (Module (Environment address, address))))) effects
            , Member (Reader PackageInfo) effects
            , Member (Reader Span) effects
            , Member (Resumable (AddressError address value)) effects
            , Member (Resumable (EnvironmentError address)) effects
            , Member (Resumable EvalError) effects
            , Member (Resumable ResolutionError) effects
            , Member (Resumable (Unspecialized value)) effects
            , Member (State (Heap address (Cell address) value)) effects
            , Member Trace effects
            , Recursive term
            , Reducer value (Cell address value)
            , ValueRoots address value
            , inner ~ (Exc (LoopControl address) ': Exc (Return address) ': Env address ': Allocator address value ': Reader ModuleInfo ': effects)
            )
         => proxy lang
         -> (SubtermAlgebra Module      term (TermEvaluator term address value inner address)            -> SubtermAlgebra Module      term (TermEvaluator term address value inner address))
         -> (SubtermAlgebra (Base term) term (TermEvaluator term address value inner (ValueRef address)) -> SubtermAlgebra (Base term) term (TermEvaluator term address value inner (ValueRef address)))
         -> [Module term]
         -> TermEvaluator term address value effects (ModuleTable (NonEmpty (Module (Environment address, address))))
evaluate lang analyzeModule analyzeTerm modules = do
  (preludeEnv, _) <- TermEvaluator . runInModule lowerBound moduleInfoFromCallStack $ do
    definePrelude lang
    box unit
  foldr (run preludeEnv) ask modules
  where run preludeEnv m rest = do
          evaluated <- coerce
            (runInModule preludeEnv (moduleInfo m))
            (analyzeModule (subtermRef . moduleBody)
            (evalModuleBody <$> m))
          -- FIXME: this should be some sort of Monoidal insert à la the Heap to accommodate multiple Go files being part of the same module.
          local (ModuleTable.insert (modulePath (moduleInfo m)) ((evaluated <$ m) :| [])) rest

        evalModuleBody term = Subterm term (do
          result <- foldSubterms (analyzeTerm (TermEvaluator . eval . fmap (second runTermEvaluator))) term >>= TermEvaluator . address
          result <$ TermEvaluator (postlude lang))

        runInModule preludeEnv info
          = runReader info
          . runAllocator
          . runEnv preludeEnv
          . runReturn
          . runLoopControl


traceResolve :: (Show a, Show b, Member Trace effects) => a -> b -> Evaluator address value effects ()
traceResolve name path = trace ("resolved " <> show name <> " -> " <> show path)


-- Preludes

class HasPrelude (language :: Language) where
  definePrelude :: ( AbstractValue address value effects
                   , HasCallStack
                   , Member (Allocator address value) effects
                   , Member (Env address) effects
                   , Member Fresh effects
                   , Member (Reader ModuleInfo) effects
                   , Member (Reader Span) effects
                   , Member (Resumable (EnvironmentError address)) effects
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
    define "print" builtInPrint

instance HasPrelude 'Ruby where
  definePrelude _ = do
    define "puts" builtInPrint

    defineClass "Object" [] $ do
      define "inspect" (lambda (const (box (string "<object>"))))

instance HasPrelude 'TypeScript where
  definePrelude _ =
    defineNamespace "console" $ do
      define "log" builtInPrint

instance HasPrelude 'JavaScript where
  definePrelude _ = do
    defineNamespace "console" $ do
      define "log" builtInPrint

-- Postludes

class HasPostlude (language :: Language) where
  postlude :: ( AbstractValue address value effects
              , HasCallStack
              , Member (Allocator address value) effects
              , Member (Env address) effects
              , Member Fresh effects
              , Member (Reader ModuleInfo) effects
              , Member (Reader Span) effects
              , Member (Resumable (EnvironmentError address)) effects
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
  FreeVariablesError :: [Name] -> EvalError Name
  -- Indicates that our evaluator wasn't able to make sense of these literals.
  IntegerFormatError  :: Text -> EvalError Integer
  FloatFormatError    :: Text -> EvalError Scientific
  RationalFormatError :: Text -> EvalError Rational
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

runEvalError :: (Effectful m, Effects effects) => m (Resumable EvalError ': effects) a -> m effects (Either (SomeExc EvalError) a)
runEvalError = runResumable

runEvalErrorWith :: (Effectful m, Effects effects) => (forall resume . EvalError resume -> m effects resume) -> m (Resumable EvalError ': effects) a -> m effects a
runEvalErrorWith = runResumableWith


data Unspecialized a b where
  Unspecialized :: String -> Unspecialized value value

deriving instance Eq (Unspecialized a b)
deriving instance Show (Unspecialized a b)

instance Eq1 (Unspecialized a) where
  liftEq _ (Unspecialized a) (Unspecialized b) = a == b

instance Show1 (Unspecialized a) where
  liftShowsPrec _ _ = showsPrec

runUnspecialized :: (Effectful (m value), Effects effects) => m value (Resumable (Unspecialized value) ': effects) a -> m value effects (Either (SomeExc (Unspecialized value)) a)
runUnspecialized = runResumable

runUnspecializedWith :: (Effectful (m value), Effects effects) => (forall resume . Unspecialized value resume -> m value effects resume) -> m value (Resumable (Unspecialized value) ': effects) a -> m value effects a
runUnspecializedWith = runResumableWith


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
