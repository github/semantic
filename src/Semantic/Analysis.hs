{-# LANGUAGE TypeFamilies, TypeOperators #-}
module Semantic.Analysis
( evaluate
, runDomainEffects
, evalTerm
) where

import Control.Abstract as Abstract
import Control.Abstract.ScopeGraph (runAllocator)
import Control.Effect.Interpose
import Data.Abstract.Evaluatable
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Function
import Data.Language (Language)
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

type DomainC term address value m
  = FunctionC term address value                                          (Eff
  ( WhileC value                                                          (Eff
  ( BooleanC value                                                        (Eff
  ( StringC value                                                         (Eff
  ( NumericC value                                                        (Eff
  ( BitwiseC value                                                        (Eff
  ( ObjectC address value                                                 (Eff
  ( UnitC value                                                           (Eff
  ( InterposeC (Resumable (BaseError (UnspecializedError address value))) (Eff
    m)))))))))))))))))

-- | Evaluate a list of modules with the prelude for the passed language available, and applying the passed function to every module.
evaluate :: ( Carrier outerSig outer
            , derefSig ~ (Deref value :+: allocatorSig)
            , derefC ~ (DerefC address value (Eff allocatorC))
            , Carrier derefSig derefC
            , allocatorSig ~ (Allocator address :+: Reader ModuleInfo :+: outerSig)
            , allocatorC ~ (AllocatorC address (Eff (ReaderC ModuleInfo (Eff outer))))
            , Carrier allocatorSig allocatorC
            , Effect outerSig
            , Member Fresh outerSig
            , Member (Reader (ModuleTable (Module (ModuleResult address value)))) outerSig
            , Member (State (Heap address address value)) outerSig
            , Member (State (ScopeGraph address)) outerSig
            , Ord address
            )
         => proxy (lang :: Language)
         -> (Module (Either (proxy lang) term) -> Evaluator term address value (ModuleC address value outer) value)
         -> [Module term]
         -> Evaluator term address value outer (ModuleTable (Module (ModuleResult address value)))
evaluate lang runModule modules = do
  let prelude = Module moduleInfoFromCallStack (Left lang)
  ((preludeScopeAddress, preludeFrameAddress), _) <- evalModule Nothing Nothing prelude
  foldr (run preludeScopeAddress preludeFrameAddress . fmap Right) ask modules
  where run preludeScopeAddress preludeFrameAddress m rest = do
          evaluated <- evalModule (Just preludeScopeAddress) (Just preludeFrameAddress) m
          local (ModuleTable.insert (modulePath (moduleInfo m)) (evaluated <$ m)) rest

        -- Run the allocator and Reader ModuleInfo effects (Some allocator instances depend on Reader ModuleInfo)
        -- after setting up the scope and frame for a module.
        evalModule parentScope parentFrame m = raiseHandler (runReader (moduleInfo m)) . runAllocator $ do
          let (scopeEdges, frameLinks) = case (parentScope, parentFrame) of
                (Just parentScope, Just parentFrame) -> (Map.singleton Lexical [ parentScope ], Map.singleton Lexical (Map.singleton parentScope parentFrame))
                _ -> mempty
          scopeAddress <- newScope scopeEdges
          frameAddress <- newFrame scopeAddress frameLinks
          val <- runInModule scopeAddress frameAddress m
          pure ((scopeAddress, frameAddress), val)
          where runInModule scopeAddress frameAddress
                  = runDeref
                  . raiseHandler (runReader (CurrentFrame frameAddress))
                  . raiseHandler (runReader (CurrentScope scopeAddress))
                  . runReturn
                  . runLoopControl
                  . runModule

runDomainEffects :: ( AbstractValue term address value (DomainC term address value m)
                    , Carrier sig m
                    , unitC ~ UnitC value (Eff (InterposeC (Resumable (BaseError (UnspecializedError address value))) (Eff m)))
                    , unitSig ~ (Unit value :+: Interpose (Resumable (BaseError (UnspecializedError address value))) :+: sig)
                    , Carrier unitSig unitC
                    , objectC ~ ObjectC address value (Eff unitC)
                    , objectSig ~ (Abstract.Object address value :+: unitSig)
                    , Carrier objectSig objectC
                    , bitwiseC ~ BitwiseC value (Eff objectC)
                    , bitwiseSig ~ (Abstract.Bitwise value :+: objectSig)
                    , Carrier bitwiseSig bitwiseC
                    , numericC ~ NumericC value (Eff bitwiseC)
                    , numericSig ~ (Abstract.Numeric value :+: bitwiseSig)
                    , Carrier numericSig numericC
                    , stringC ~ StringC value (Eff numericC)
                    , stringSig ~ (Abstract.String value :+: numericSig)
                    , Carrier stringSig stringC
                    , booleanC ~ BooleanC value (Eff stringC)
                    , booleanSig ~ (Boolean value :+: stringSig)
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
                    , Member Fresh sig
                    , Member (Reader (CurrentFrame address)) sig
                    , Member (Reader (CurrentScope address)) sig
                    , Member (Reader ModuleInfo) sig
                    , Member (Reader Span) sig
                    , Member (Resumable (BaseError (AddressError address value))) sig
                    , Member (Resumable (BaseError (HeapError address))) sig
                    , Member (Resumable (BaseError (ScopeError address))) sig
                    , Member (Resumable (BaseError (UnspecializedError address value))) sig
                    , Member (State (Heap address address value)) sig
                    , Member (State (ScopeGraph address)) sig
                    , Member Trace sig
                    , Ord address
                    , Show address
                    )
                 => (term -> Evaluator term address value (DomainC term address value m) value)
                 -> Module (Either (proxy lang) term)
                 -> Evaluator term address value m value
runDomainEffects runTerm
  = raiseHandler runInterpose
  . runUnit
  . runObject
  . runBitwise
  . runNumeric
  . runString
  . runBoolean
  . runWhile
  . runFunction runTerm
  . either ((unit <*) . definePrelude) runTerm
  . moduleBody

-- | Evaluate a term recursively, applying the passed function at every recursive position.
--
--   This calls out to the 'Evaluatable' instances, and can have other functions composed after it to e.g. intercept effects arising in the evaluation of the term.
evalTerm :: ( Carrier sig m
            , Declarations term
            , Evaluatable (Base term)
            , FreeVariables term
            , AbstractValue term address value m
            , Member (Allocator address) sig
            , Member (Bitwise value) sig
            , Member (Boolean value) sig
            , Member (Deref value) sig
            , Member (Error (LoopControl value)) sig
            , Member (Error (Return value)) sig
            , Member (Function term address value) sig
            , Member (Modules address value) sig
            , Member (Numeric value) sig
            , Member (Reader ModuleInfo) sig
            , Member (Reader PackageInfo) sig
            , Member (Reader Span) sig
            , Member (Resumable (BaseError (AddressError address value))) sig
            , Member (Resumable (BaseError (HeapError address))) sig
            , Member (Resumable (BaseError (ScopeError address))) sig
            , Member (Resumable (BaseError (UnspecializedError address value))) sig
            , Member (Resumable (BaseError (EvalError term address value))) sig
            , Member (Resumable (BaseError ResolutionError)) sig
            , Member (State (Heap address address value)) sig
            , Member (State (ScopeGraph address)) sig
            , Member (Abstract.String value) sig
            , Member (Reader (CurrentFrame address)) sig
            , Member (Reader (CurrentScope address)) sig
            , Member (State Span) sig
            , Member (Unit value) sig
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
