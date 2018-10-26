{-# LANGUAGE TypeFamilies, TypeOperators #-}
module Semantic.Analysis
( ModuleC
, ValueC
, evaluate
, evalModule
, evalTerm
, runInModule
, runInTerm
, evaluateModules
) where

import Control.Abstract
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
  ( InterposeC (Resumable (BaseError (UnspecializedError value))) (Eff
    m)))))))

evaluate :: ( Carrier sig m
            , Member (Reader (ModuleTable (NonEmpty (Module (ModuleResult address))))) sig
            )
         => lang
         -> (Bindings address -> Module (Either lang term) -> Evaluator term address value m (ModuleResult address))
         -> [Module term]
         -> Evaluator term address value m (ModuleTable (NonEmpty (Module (ModuleResult address))))
evaluate lang evalModule modules = do
  let prelude = Module moduleInfoFromCallStack (Left lang)
  Module _ (_, (preludeBinds, _)) <- run lowerBound prelude
  evaluateModules (run preludeBinds . fmap Right <$> modules)
  where run prelude m = (<$ m) <$> evalModule prelude m

evalModule :: ( AbstractValue term address value (ValueC term address value inner)
              , Carrier sig m
              , Carrier innerSig inner
              , functionSig ~ (Function term address value :+: whileSig)
              , functionC ~ FunctionC term address value (Eff whileC)
              , Carrier functionSig functionC
              , whileSig ~ (While value :+: booleanSig)
              , whileC ~ WhileC value (Eff booleanC)
              , Carrier whileSig whileC
              , booleanSig ~ (Boolean value :+: Interpose (Resumable (BaseError (UnspecializedError value))) :+: innerSig)
              , booleanC ~ BooleanC value (Eff (InterposeC (Resumable (BaseError (UnspecializedError value))) (Eff inner)))
              , Carrier booleanSig booleanC
              , derefSig ~ (Deref value :+: allocatorSig)
              , derefC ~ (DerefC address value (Eff allocatorC))
              , Carrier derefSig derefC
              , allocatorSig ~ (Allocator address :+: Reader ModuleInfo :+: sig)
              , allocatorC ~ (AllocatorC address (Eff (ReaderC ModuleInfo (Eff m))))
              , Carrier allocatorSig allocatorC
              , Effect sig
              , HasPrelude language
              , Member Fresh sig
              , Member (Allocator address) innerSig
              , Member (Deref value) innerSig
              , Member (Env address) innerSig
              , Member Fresh innerSig
              , Member (Reader ModuleInfo) innerSig
              , Member (Reader Span) innerSig
              , Member (Resumable (BaseError (AddressError address value))) innerSig
              , Member (Resumable (BaseError (UnspecializedError value))) innerSig
              , Member (Resumable (BaseError (EnvironmentError address))) innerSig
              , Member (State (Heap address value)) innerSig
              , Member Trace innerSig
              , Ord address
              )
           => (  (Module (Either (proxy language) term) -> Evaluator term address value inner address)
              -> (Module body                           -> Evaluator term address value (ModuleC address value m) address))
           -> (term -> Evaluator term address value (ValueC term address value inner) address)
           -> Bindings address
           -> Module body
           -> Evaluator term address value m (ModuleResult address)
evalModule perModule perTerm prelude m = runInModule prelude (moduleInfo m) (perModule (runInTerm perTerm . moduleBody) m)

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

runInTerm :: ( AbstractValue term address value (ValueC term address value m)
             , Carrier sig m
             , booleanC ~ BooleanC value (Eff (InterposeC (Resumable (BaseError (UnspecializedError value))) (Eff m)))
             , booleanSig ~ (Boolean value :+: Interpose (Resumable (BaseError (UnspecializedError value))) :+: sig)
             , Carrier booleanSig booleanC
             , whileC ~ WhileC value (Eff booleanC)
             , whileSig ~ (While value :+: booleanSig)
             , Carrier whileSig whileC
             , functionC ~ FunctionC term address value (Eff whileC)
             , functionSig ~ (Function term address value :+: whileSig)
             , Carrier functionSig functionC
             , HasPrelude lang
             , Member (Allocator address) sig
             , Member (Deref value) sig
             , Member (Env address) sig
             , Member Fresh sig
             , Member (Reader ModuleInfo) sig
             , Member (Reader Span) sig
             , Member (Resumable (BaseError (AddressError address value))) sig
             , Member (Resumable (BaseError (EnvironmentError address))) sig
             , Member (Resumable (BaseError (UnspecializedError value))) sig
             , Member (State (Heap address value)) sig
             , Member Trace sig
             , Ord address
             )
          => (term -> Evaluator term address value (ValueC term address value m) address)
          -> Either (proxy lang) term
          -> Evaluator term address value m address
runInTerm evalTerm = raiseHandler runInterpose . runBoolean . runWhile . runFunction evalTerm . either ((*> box unit) . definePrelude) evalTerm

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
