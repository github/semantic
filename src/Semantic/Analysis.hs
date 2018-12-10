{-# LANGUAGE TypeFamilies, TypeOperators #-}
module Semantic.Analysis
( evaluate
, evalTerm
) where

import Control.Abstract
import Control.Abstract.ScopeGraph (runAllocator)
import Control.Effect.Interpose
import Data.Abstract.Evaluatable
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Function
import Prologue
import qualified Data.Map.Strict as Map

type ModuleC address value m
  = ErrorC (LoopControl value)     (Eff
  ( ErrorC (Return value)          (Eff
  ( ReaderC (CurrentScope address) (Eff
  ( ReaderC (CurrentFrame address) (Eff
  ( DerefC address value           (Eff
  ( AllocatorC address             (Eff
  ( ReaderC ModuleInfo             (Eff
    m)))))))))))))

type ValueC term address value m
  = FunctionC term address value                                  (Eff
  ( WhileC value                                                  (Eff
  ( BooleanC value                                                (Eff
  ( InterposeC (Resumable (BaseError (UnspecializedError value))) (Eff
    m)))))))

-- | Evaluate a list of modules with the prelude for the passed language available, and applying the passed function to every module.
evaluate :: ( AbstractValue term address value (ValueC term address value inner)
            , Carrier innerSig inner
            , Carrier outerSig outer
            , derefSig ~ (Deref value :+: allocatorSig)
            , derefC ~ (DerefC address value (Eff allocatorC))
            , Carrier derefSig derefC
            , allocatorSig ~ (Allocator address :+: Reader ModuleInfo :+: outerSig)
            , allocatorC ~ (AllocatorC address (Eff (ReaderC ModuleInfo (Eff outer))))
            , Carrier allocatorSig allocatorC
            , booleanC ~ BooleanC value (Eff (InterposeC (Resumable (BaseError (UnspecializedError value))) (Eff inner)))
            , booleanSig ~ (Boolean value :+: Interpose (Resumable (BaseError (UnspecializedError value))) :+: innerSig)
            , Carrier booleanSig booleanC
            , whileC ~ WhileC value (Eff booleanC)
            , whileSig ~ (While value :+: booleanSig)
            , Carrier whileSig whileC
            , functionC ~ FunctionC term address value (Eff whileC)
            , functionSig ~ (Function term address value :+: whileSig)
            , Carrier functionSig functionC
            , Effect outerSig
            , HasPrelude lang
            , Member Fresh outerSig
            , Member (Allocator address) innerSig
            , Member (Deref value) innerSig
            , Member Fresh innerSig
            , Member (Reader ModuleInfo) innerSig
            , Member (Reader (ModuleTable (NonEmpty (Module (ModuleResult address value))))) outerSig
            , Member (Reader Span) innerSig
            , Member (Resumable (BaseError (AddressError address value))) innerSig
            , Member (Resumable (BaseError (UnspecializedError value))) innerSig
            , Member (State (Heap address address value)) innerSig
            , Member (State (ScopeGraph address)) innerSig
            , Member (State (Heap address address value)) outerSig
            , Member (State (ScopeGraph address)) outerSig
            , Member (Reader (CurrentFrame address)) innerSig
            , Member (Reader (CurrentScope address)) innerSig
            , Member (Resumable (BaseError (HeapError address))) innerSig
            , Member (Resumable (BaseError (ScopeError address))) innerSig
            , Member Trace innerSig
            , Ord address
            , Show address
            )
         => proxy lang
         -> (  (Module (Either (proxy lang) term) -> Evaluator term address value inner value)
            -> (Module (Either (proxy lang) term) -> Evaluator term address value (ModuleC address value outer) value))
         -> (term -> Evaluator term address value (ValueC term address value inner) value)
         -> [Module term]
         -> Evaluator term address value outer (ModuleTable (NonEmpty (Module (ModuleResult address value))))
evaluate lang perModule runTerm modules = do
  let prelude = Module moduleInfoFromCallStack (Left lang)
  ((preludeScopeAddress, preludeFrameAddress), _) <- evalModule Nothing Nothing prelude
  foldr (run preludeScopeAddress preludeFrameAddress . fmap Right) ask modules
  where run preludeScopeAddress preludeFrameAddress m rest = do
          evaluated <- evalModule (Just preludeScopeAddress) (Just preludeFrameAddress) m
          -- FIXME: this should be some sort of Monoidal insert à la the Heap to accommodate multiple Go files being part of the same module.
          local (ModuleTable.insert (modulePath (moduleInfo m)) ((evaluated <$ m) :| [])) rest

        -- Run the allocator and Reader ModuleInfo effects (Some allocator instances depend on Reader ModuleInfo)
        -- after setting up the scope and frame for a module.
        evalModule parentScope parentFrame m = raiseHandler (runReader (moduleInfo m)) . runAllocator $ do
          let (scopeEdges, frameLinks) = case (parentScope, parentFrame) of
                (Just parentScope, Just parentFrame) -> (Map.singleton Lexical [ parentScope ], Map.singleton Lexical (Map.singleton parentScope parentFrame))
                _ -> mempty
          scopeAddress <- newScope scopeEdges
          frameAddress <- newFrame scopeAddress frameLinks
          val <- runInModule scopeAddress frameAddress (perModule (runValueEffects . moduleBody) m)
          pure ((scopeAddress, frameAddress), val)
          where runInModule scopeAddress frameAddress
                  = runDeref
                  . raiseHandler (runReader (CurrentFrame frameAddress))
                  . raiseHandler (runReader (CurrentScope scopeAddress))
                  . runReturn
                  . runLoopControl

        runValueEffects = raiseHandler runInterpose . runBoolean . runWhile . runFunction runTerm . either ((unit <$) . definePrelude) runTerm

-- | Evaluate a term recursively, applying the passed function at every recursive position.
--
--   This calls out to the 'Evaluatable' instances, will be passed to 'runValueEffects', and can have other functions composed after it to e.g. intercept effects arising in the evaluation of the term.
evalTerm :: ( Carrier sig m
            , Declarations term
            , Evaluatable (Base term)
            , FreeVariables term
            , AbstractValue term address value m
            , Member (Allocator address) sig
            , Member (Boolean value) sig
            , Member (Deref value) sig
            , Member (Error (LoopControl value)) sig
            , Member (Error (Return value)) sig
            , Member (Function term address value) sig
            , Member (Modules address value) sig
            , Member (Reader ModuleInfo) sig
            , Member (Reader PackageInfo) sig
            , Member (Reader Span) sig
            , Member (Resumable (BaseError (AddressError address value))) sig
            , Member (Resumable (BaseError (HeapError address))) sig
            , Member (Resumable (BaseError (ScopeError address))) sig
            , Member (Resumable (BaseError (UnspecializedError value))) sig
            , Member (Resumable (BaseError (EvalError address value))) sig
            , Member (Resumable (BaseError ResolutionError)) sig
            , Member (State (Heap address address value)) sig
            , Member (State (ScopeGraph address)) sig
            , Member (Reader (CurrentFrame address)) sig
            , Member (Reader (CurrentScope address)) sig
            , Member (State Span) sig
            , Member (While value) sig
            , Member Fresh sig
            , Member Trace sig
            , Ord address
            , Show address
            , Recursive term
            )
         => Open (term -> Evaluator term address value m value)
         -> term -> Evaluator term address value m value
-- NB: We use a lazy pattern match for the lambda’s argument to postpone evaluating the pair until eval/ref is called.
evalTerm perTerm = fst (fix (\ ~(ev, re) -> (perTerm (eval ev re . project), ref ev re . project)))
