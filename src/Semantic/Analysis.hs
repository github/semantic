{-# LANGUAGE DataKinds, FlexibleContexts, TypeFamilies, TypeOperators #-}
module Semantic.Analysis
( evaluate
, runDomainEffects
, evalTerm
) where

import Prologue

import qualified Data.Map.Strict as Map

import Control.Abstract as Abstract
import Control.Abstract.ScopeGraph (runAllocator)
import Control.Effect.Carrier
import Control.Effect.Interpose
import Data.Abstract.Evaluatable
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Language (Language)
import Source.Span

type ModuleC address value m
  = ErrorC (LoopControl value)
  ( ErrorC (Return value)
  ( ReaderC (CurrentScope address)
  ( ReaderC (CurrentFrame address)
  ( DerefC address value
  ( AllocatorC address
  ( ReaderC ModuleInfo
    m))))))

type DomainC term address value m
  = FunctionC term address value
  ( WhileC value
  ( BooleanC value
  ( StringC value
  ( NumericC value
  ( BitwiseC value
  ( ObjectC address value
  ( ArrayC value
  ( HashC value
  ( UnitC value
  ( InterposeC (Resumable (BaseError (UnspecializedError address value)))
    m))))))))))

-- | Evaluate a list of modules with the prelude for the passed language available, and applying the passed function to every module.
evaluate :: ( Carrier outerSig outer
            , derefSig ~ (Deref value :+: allocatorSig)
            , derefC ~ DerefC address value allocatorC
            , Carrier derefSig derefC
            , allocatorSig ~ (Allocator address :+: Reader ModuleInfo :+: outerSig)
            , allocatorC ~ AllocatorC address (ReaderC ModuleInfo outer)
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
          scopeAddress <- if Prologue.null scopeEdges then newPreludeScope scopeEdges else newScope scopeEdges
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
                    , unitC ~ UnitC value (InterposeC (Resumable (BaseError (UnspecializedError address value))) m)
                    , unitSig ~ (Unit value :+: Interpose (Resumable (BaseError (UnspecializedError address value))) :+: sig)
                    , hashC ~ HashC value unitC
                    , hashSig ~ (Abstract.Hash value :+: unitSig)
                    , arrayC ~ ArrayC value hashC
                    , arraySig ~ (Abstract.Array value :+: hashSig)
                    , objectC ~ ObjectC address value arrayC
                    , objectSig ~ (Abstract.Object address value :+: arraySig)
                    , bitwiseC ~ BitwiseC value objectC
                    , bitwiseSig ~ (Abstract.Bitwise value :+: objectSig)
                    , numericC ~ NumericC value bitwiseC
                    , numericSig ~ (Abstract.Numeric value :+: bitwiseSig)
                    , stringC ~ StringC value numericC
                    , stringSig ~ (Abstract.String value :+: numericSig)
                    , booleanC ~ BooleanC value stringC
                    , booleanSig ~ (Boolean value :+: stringSig)
                    , whileC ~ WhileC value booleanC
                    , whileSig ~ (While value :+: booleanSig)
                    , functionC ~ FunctionC term address value whileC
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
  . runHash
  . runArray
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
            , AbstractValue term address value m
            , AccessControls term
            , Declarations term
            , Evaluatable (Base term)
            , FreeVariables term
            , HasSpan term
            , Member (Allocator address) sig
            , Member (Bitwise value) sig
            , Member (Boolean value) sig
            , Member (Deref value) sig
            , Member (Error (LoopControl value)) sig
            , Member (Error (Return value)) sig
            , Member (Function term address value) sig
            , Member (Modules address value) sig
            , Member (Numeric value) sig
            , Member (Object address value) sig
            , Member (Array value) sig
            , Member (Hash value) sig
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
            , Recursive term
            , Show address
            )
         => Open (term -> Evaluator term address value m value)
         -> term -> Evaluator term address value m value
-- NB: We use a lazy pattern match for the lambda’s argument to postpone evaluating the pair until eval/ref is called.
evalTerm perTerm = fst (fix (\ ~(ev, re) -> (perTerm (eval ev re . project), ref ev re . project)))
