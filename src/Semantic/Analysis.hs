{-# LANGUAGE TypeFamilies, TypeOperators #-}
module Semantic.Analysis
( ModuleC
, ValueC
, evaluate
, evalTerm
, runInModule
, runInTerm
, evaluateModules
) where

import Control.Abstract
import Control.Effect.Eavesdrop
import Control.Effect.Interpose
import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Function
import Prologue

type ModuleC address value m
  = ErrorC (LoopControl address) (Eff
  ( ErrorC (Return address)      (Eff
  ( EnvC address                 (Eff
  ( ScopeEnvC address            (Eff
  ( DerefC address value         (Eff
  ( AllocatorC address           (Eff
  ( ReaderC ModuleInfo           (Eff
    m)))))))))))))

type ValueC term address value m
  = FunctionC term address value                                  (Eff
  ( WhileC value                                                  (Eff
  ( BooleanC value                                                (Eff
  ( EavesdropC (Modules address)                                  (Eff
  ( InterposeC (Resumable (BaseError (UnspecializedError value))) (Eff
    m)))))))))

evaluate :: ( AbstractValue term address value valueC
            , Carrier sig c
            , allocatorC ~ AllocatorC address (Eff (ReaderC ModuleInfo (Eff c)))
            , Carrier (Allocator address :+: Reader ModuleInfo :+: sig) allocatorC
            , Carrier (Deref value :+: Allocator address :+: Reader ModuleInfo :+: sig) (DerefC address value (Eff allocatorC))
            , booleanC ~ BooleanC value (Eff (EavesdropC (Modules address) (Eff (InterposeC (Resumable (BaseError (UnspecializedError value))) (Eff moduleC)))))
            , Carrier (Boolean value :+: moduleSig) booleanC
            , whileC ~ WhileC value (Eff booleanC)
            , moduleSig ~ (Eavesdrop (Modules address) :+: Interpose (Resumable (BaseError (UnspecializedError value))) :+: Error (LoopControl address) :+: Error (Return address) :+: Env address :+: ScopeEnv address :+: Deref value :+: Allocator address :+: Reader ModuleInfo :+: sig)
            , Carrier (While value :+: Boolean value :+: moduleSig) whileC
            , Carrier (Function term address value :+: While value :+: Boolean value :+: moduleSig) valueC
            , Effect sig
            , HasPrelude lang
            , Member Fresh sig
            , Member (Modules address) sig
            , Member (Reader (ModuleTable (NonEmpty (Module (ModuleResult address))))) sig
            , Member (Reader Span) sig
            , Member (Resumable (BaseError (AddressError address value))) sig
            , Member (Resumable (BaseError (EnvironmentError address))) sig
            , Member (Resumable (BaseError (UnspecializedError value))) sig
            , Member (State (Heap address value)) sig
            , Member Trace sig
            , Ord address
            , moduleC ~ ModuleC address value c
            , valueC ~ ValueC term address value moduleC
            )
         => proxy lang
         -> Open (Module term -> Evaluator term address value moduleC address)
         -> (term -> Evaluator term address value valueC address)
         -> [Module term]
         -> Evaluator term address value c (ModuleTable (NonEmpty (Module (ModuleResult address))))
evaluate lang analyzeModule evalTerm modules = do
  (_, (preludeBinds, _)) <- runInModule lowerBound moduleInfoFromCallStack . runInTerm evalTerm $ do
    definePrelude lang
    box unit
  evaluateModules (run preludeBinds <$> modules)
  where run preludeBinds m = (<$ m) <$> runInModule preludeBinds (moduleInfo m) (analyzeModule (runInTerm evalTerm . evalTerm . moduleBody) m)

evalTerm :: ( Carrier sig m
            , Declarations term
            , Evaluatable (Base term)
            , FreeVariables term
            , AbstractValue term address value m
            , Member (Allocator address) sig
            , Member (Boolean value) sig
            , Member (Deref value) sig
            , Member (Env address) sig
            , Member (Error (LoopControl address)) sig
            , Member (Error (Return address)) sig
            , Member Fresh sig
            , Member (Function term address value) sig
            , Member (Modules address) sig
            , Member (Reader ModuleInfo) sig
            , Member (Reader PackageInfo) sig
            , Member (Reader Span) sig
            , Member (Resumable (BaseError (AddressError address value))) sig
            , Member (Resumable (BaseError (EnvironmentError address))) sig
            , Member (Resumable (BaseError EvalError)) sig
            , Member (Resumable (BaseError ResolutionError)) sig
            , Member (Resumable (BaseError (UnspecializedError value))) sig
            , Member (ScopeEnv address) sig
            , Member (State (Heap address value)) sig
            , Member (State Span) sig
            , Member Trace sig
            , Member (While value) sig
            , Ord address
            , Recursive term
            )
         => Open (Open (term -> Evaluator term address value m (ValueRef address)))
         -> term -> Evaluator term address value m address
evalTerm analyzeTerm = fix (analyzeTerm (\ ev -> eval ev . project)) >=> address

runInModule :: ( Carrier sig m
               , allocatorC ~ (AllocatorC address (Eff (ReaderC ModuleInfo (Eff m))))
               , allocatorSig ~ (Allocator address :+: Reader ModuleInfo :+: sig)
               , Carrier allocatorSig allocatorC
               , Carrier (Deref value :+: allocatorSig) (DerefC address value (Eff allocatorC))
               , Effect sig
               , Member Fresh sig
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
  . runEnv (EvalContext Nothing (Env.push (newEnv prelude)))
  . runReturn
  . runLoopControl

runInTerm :: ( Carrier sig m
             , booleanC ~ BooleanC value (Eff (EavesdropC (Modules address) (Eff (InterposeC (Resumable (BaseError (UnspecializedError value))) (Eff m)))))
             , booleanSig ~ (Boolean value :+: Eavesdrop (Modules address) :+: Interpose (Resumable (BaseError (UnspecializedError value))) :+: sig)
             , Carrier booleanSig booleanC
             , whileC ~ WhileC value (Eff booleanC)
             , whileSig ~ (While value :+: booleanSig)
             , Carrier whileSig whileC
             , functionC ~ FunctionC term address value (Eff whileC)
             , functionSig ~ (Function term address value :+: whileSig)
             , Carrier functionSig functionC
             , Member (Modules address) sig
             , Member (Resumable (BaseError (UnspecializedError value))) sig
             )
          => (term -> Evaluator term address value (ValueC term address value m) address)
          -> Evaluator term address value (ValueC term address value m) a
          -> Evaluator term address value m a
runInTerm evalTerm = raiseHandler runInterpose . raiseHandler runEavesdrop . runBoolean . runWhile . runFunction evalTerm

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
