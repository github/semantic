{-# LANGUAGE GADTs, KindSignatures, RankNTypes, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Evaluatable
( module X
, Evaluatable(..)
, ModuleC
, ValueC
, evaluate
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
import Control.Abstract.Environment as X hiding (runEnvironmentError, runEnvironmentErrorWith)
import Control.Abstract.Evaluator as X hiding (LoopControl(..), Return(..), catchLoopControl, runLoopControl, catchReturn, runReturn)
import Control.Abstract.Heap as X hiding (runAddressError, runAddressErrorWith)
import Control.Abstract.Modules as X (Modules, ModuleResult, ResolutionError(..), load, lookupModule, listModulesInDir, require, resolve, throwResolutionError)
import Control.Abstract.Value as X hiding (Boolean(..), Function(..), While(..))
import Control.Effect.Eavesdrop
import Control.Effect.Interpose
import Data.Abstract.Declarations as X
import Data.Abstract.Environment as X
import Data.Abstract.BaseError as X
import Data.Abstract.FreeVariables as X
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Abstract.Name as X
import Data.Abstract.Ref as X
import Data.Language
import Data.Function
import Data.Scientific (Scientific)
import Data.Semigroup.App
import Data.Semigroup.Foldable
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
          , Member (ScopeEnv address) sig
          , Member (Env address) sig
          , Member (Error (LoopControl address)) sig
          , Member (Error (Return address)) sig
          , Member Fresh sig
          , Member (Function term address value) sig
          , Member (Modules address) sig
          , Member (Reader ModuleInfo) sig
          , Member (Reader PackageInfo) sig
          , Member (Reader Span) sig
          , Member (State Span) sig
          , Member (Resumable (BaseError (AddressError address value))) sig
          , Member (Resumable (BaseError (EnvironmentError address))) sig
          , Member (Resumable (BaseError (UnspecializedError value))) sig
          , Member (Resumable (BaseError EvalError)) sig
          , Member (Resumable (BaseError ResolutionError)) sig
          , Member (State (Heap address value)) sig
          , Member Trace sig
          , Ord address
          )
       => (term -> Evaluator term address value m (ValueRef address))
       -> (constr term -> Evaluator term address value m (ValueRef address))
  eval recur expr = do
    traverse_ recur expr
    v <- throwUnspecializedError $ UnspecializedError ("Eval unspecialized for " <> liftShowsPrec (const (const id)) (const id) 0 expr "")
    rvalBox v


type ModuleC address value m
  = EavesdropC (Modules address)                                  (Eff
  ( InterposeC (Resumable (BaseError (UnspecializedError value))) (Eff
  ( ErrorC (LoopControl address)                                  (Eff
  ( ErrorC (Return address)                                       (Eff
  ( EnvC address                                                  (Eff
  ( ScopeEnvC address                                             (Eff
  ( DerefC address value                                          (Eff
  ( AllocatorC address                                            (Eff
  ( ReaderC ModuleInfo                                            (Eff
    m)))))))))))))))))

type ValueC term address value m
  = FunctionC term address value (Eff
  ( WhileC value                 (Eff
  ( BooleanC value               (Eff
    m)))))

evaluate :: ( AbstractValue term address value valueC
            , Carrier sig c
            , allocatorC ~ AllocatorC address (Eff (ReaderC ModuleInfo (Eff c)))
            , Carrier (Allocator address :+: Reader ModuleInfo :+: sig) allocatorC
            , Carrier (Deref value :+: Allocator address :+: Reader ModuleInfo :+: sig) (DerefC address value (Eff allocatorC))
            , booleanC ~ BooleanC value (Eff moduleC)
            , Carrier (Boolean value :+: moduleSig) booleanC
            , whileC ~ WhileC value (Eff booleanC)
            , moduleSig ~ (Eavesdrop (Modules address) :+: Interpose (Resumable (BaseError (UnspecializedError value))) :+: Error (LoopControl address) :+: Error (Return address) :+: Env address :+: ScopeEnv address :+: Deref value :+: Allocator address :+: Reader ModuleInfo :+: sig)
            , Carrier (While value :+: Boolean value :+: moduleSig) whileC
            , Carrier (Function term address value :+: While value :+: Boolean value :+: moduleSig) valueC
            , Declarations term
            , Effect sig
            , Evaluatable (Base term)
            , FreeVariables term
            , HasPrelude lang
            , Member Fresh sig
            , Member (Modules address) sig
            , Member (Reader (ModuleTable (NonEmpty (Module (ModuleResult address))))) sig
            , Member (Reader PackageInfo) sig
            , Member (Reader Span) sig
            , Member (State Span) sig
            , Member (Resumable (BaseError (AddressError address value))) sig
            , Member (Resumable (BaseError (EnvironmentError address))) sig
            , Member (Resumable (BaseError EvalError)) sig
            , Member (Resumable (BaseError ResolutionError)) sig
            , Member (Resumable (BaseError (UnspecializedError value))) sig
            , Member (State (Heap address value)) sig
            , Member Trace sig
            , Ord address
            , Recursive term
            , moduleC ~ ModuleC address value c
            , valueC ~ ValueC term address value moduleC
            )
         => proxy lang
         -> Open (Module term -> Evaluator term address value moduleC address)
         -> Open (Open (term -> Evaluator term address value valueC (ValueRef address)))
         -> [Module term]
         -> Evaluator term address value c (ModuleTable (NonEmpty (Module (ModuleResult address))))
evaluate lang analyzeModule analyzeTerm modules = do
  (_, (preludeBinds, _)) <- runInModule lowerBound moduleInfoFromCallStack . runValue $ do
    definePrelude lang
    box unit
  evaluateModules (run preludeBinds <$> modules)
  where run preludeBinds m = (<$ m) <$> runInModule preludeBinds (moduleInfo m) (analyzeModule (runValue . evalTerm . moduleBody) m)

        evalTerm = fix (analyzeTerm (\ ev -> eval ev . project)) >=> address

        runValue = runBoolean . runWhile . runFunction evalTerm

runInModule :: ( Carrier sig m
               , allocatorC ~ (AllocatorC address (Eff (ReaderC ModuleInfo (Eff m))))
               , allocatorSig ~ (Allocator address :+: Reader ModuleInfo :+: sig)
               , Carrier allocatorSig allocatorC
               , Carrier (Deref value :+: allocatorSig) (DerefC address value (Eff allocatorC))
               , Effect sig
               , Member Fresh sig
               , Member (Modules address) sig
               , Member (Resumable (BaseError (UnspecializedError value))) sig
               , Ord address
               )
            => Bindings address
            -> ModuleInfo
            -> Evaluator term address value (ModuleC address value m) address
            -> Evaluator term address value m (ModuleResult address)
runInModule prelude info
  = raiseHandler (runReader info)
  . runAllocator
  . runDeref
  . runScopeEnv
  . runEnv (EvalContext Nothing (X.push (newEnv prelude)))
  . runReturn
  . runLoopControl
  . raiseHandler runInterpose
  . raiseHandler runEavesdrop

evaluateModules :: ( Carrier sig m
                   , Member (Reader (ModuleTable (NonEmpty (Module (ModuleResult address))))) sig
                   )
         => [Evaluator term address value m (Module (ModuleResult address))]
         -> Evaluator term address value m (ModuleTable (NonEmpty (Module (ModuleResult address))))
evaluateModules = foldr run ask
  where run evaluator rest = do
          evaluated <- evaluator
          -- FIXME: this should be some sort of Monoidal insert à la the Heap to accommodate multiple Go files being part of the same module.
          local (ModuleTable.insert (modulePath (moduleInfo evaluated)) (evaluated :| [])) rest


traceResolve :: (Show a, Show b, Member Trace sig, Carrier sig m) => a -> b -> Evaluator term address value m ()
traceResolve name path = trace ("resolved " <> show name <> " -> " <> show path)


-- Preludes

class HasPrelude (language :: Language) where
  definePrelude :: ( AbstractValue term address value m
                   , Carrier sig m
                   , HasCallStack
                   , Member (Allocator address) sig
                   , Member (Deref value) sig
                   , Member (Env address) sig
                   , Member Fresh sig
                   , Member (Function term address value) sig
                   , Member (Reader ModuleInfo) sig
                   , Member (Reader Span) sig
                   , Member (Resumable (BaseError (AddressError address value))) sig
                   , Member (Resumable (BaseError (EnvironmentError address))) sig
                   , Member (State (Heap address value)) sig
                   , Member Trace sig
                   , Ord address
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
    define (name "print") (builtIn Print)

instance HasPrelude 'Ruby where
  definePrelude _ = do
    define (name "puts") (builtIn Print)

    defineClass (name "Object") [] $ do
      define (name "inspect") (builtIn Show)

instance HasPrelude 'TypeScript where
  definePrelude _ =
    defineNamespace (name "console") $ do
      define (name "log") (builtIn Print)

instance HasPrelude 'JavaScript where
  definePrelude _ = do
    defineNamespace (name "console") $ do
      define (name "log") (builtIn Print)


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

instance NFData1 EvalError where
  liftRnf _ x = case x of
    NoNameError -> ()
    IntegerFormatError i -> rnf i
    FloatFormatError i -> rnf i
    RationalFormatError i -> rnf i
    DefaultExportError -> ()
    ExportError p n -> rnf p `seq` rnf n

instance NFData return => NFData (EvalError return) where
  rnf = liftRnf rnf

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

runEvalError :: (Carrier sig m, Effect sig) => Evaluator term address value (ResumableC (BaseError EvalError) (Eff m)) a -> Evaluator term address value m (Either (SomeError (BaseError EvalError)) a)
runEvalError = raiseHandler runResumable

runEvalErrorWith :: Carrier sig m => (forall resume . (BaseError EvalError) resume -> Evaluator term address value m resume) -> Evaluator term address value (ResumableWithC (BaseError EvalError) (Eff m)) a -> Evaluator term address value m a
runEvalErrorWith f = raiseHandler $ runResumableWith (runEvaluator . f)

throwEvalError :: ( Member (Reader ModuleInfo) sig
                  , Member (Reader Span) sig
                  , Member (Resumable (BaseError EvalError)) sig
                  , Carrier sig m
                  )
               => EvalError resume
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
