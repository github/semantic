{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Semantic.Analysis
( evaluate
, runDomainEffects
, evalTerm
) where


import           Control.Abstract as Abstract
import           Control.Algebra
import           Control.Carrier.Error.Either
import           Control.Carrier.Reader
import           Control.Effect.Interpose
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import           Data.Abstract.ModuleTable as ModuleTable
import           Data.Foldable
import           Data.Function
import           Data.Functor.Foldable
import           Data.Language (Language)
import qualified Data.Map.Strict as Map
import           Source.Span

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
evaluate :: ( Algebra outerSig outer
            , derefSig ~ (Deref value :+: allocatorSig)
            , derefC ~ DerefC address value allocatorC
            , Algebra derefSig derefC
            , allocatorSig ~ (Allocator address :+: Reader ModuleInfo :+: outerSig)
            , allocatorC ~ AllocatorC address (ReaderC ModuleInfo outer)
            , Algebra allocatorSig allocatorC
            , Effect outerSig
            , Has Fresh outerSig outer
            , Has (Reader (ModuleTable (Module (ModuleResult address value)))) outerSig outer
            , Has (State (Heap address address value)) outerSig outer
            , Has (State (ScopeGraph address)) outerSig outer
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
          scopeAddress <- if Data.Foldable.null scopeEdges then newPreludeScope scopeEdges else newScope scopeEdges
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
                    , Algebra functionSig functionC
                    , HasPrelude lang
                    , Has (Allocator address) sig m
                    , Has (Deref value) sig m
                    , Has Fresh sig m
                    , Has (Reader (CurrentFrame address)) sig m
                    , Has (Reader (CurrentScope address)) sig m
                    , Has (Reader ModuleInfo) sig m
                    , Has (Reader Span) sig m
                    , Has (Resumable (BaseError (AddressError address value))) sig m
                    , Has (Resumable (BaseError (HeapError address))) sig m
                    , Has (Resumable (BaseError (ScopeError address))) sig m
                    , Has (State (Heap address address value)) sig m
                    , Has (State (ScopeGraph address)) sig m
                    , Has Trace sig m
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
evalTerm :: ( AbstractValue term address value m
            , AccessControls term
            , Declarations term
            , Evaluatable (Base term)
            , FreeVariables term
            , HasSpan term
            , Has (Allocator address) sig m
            , Has (Bitwise value) sig m
            , Has (Boolean value) sig m
            , Has (Deref value) sig m
            , Has (Error (LoopControl value)) sig m
            , Has (Error (Return value)) sig m
            , Has (Function term address value) sig m
            , Has (Modules address value) sig m
            , Has (Numeric value) sig m
            , Has (Object address value) sig m
            , Has (Array value) sig m
            , Has (Hash value) sig m
            , Has (Reader ModuleInfo) sig m
            , Has (Reader PackageInfo) sig m
            , Has (Reader Span) sig m
            , Has (Resumable (BaseError (AddressError address value))) sig m
            , Has (Resumable (BaseError (HeapError address))) sig m
            , Has (Resumable (BaseError (ScopeError address))) sig m
            , Has (Resumable (BaseError (UnspecializedError address value))) sig m
            , Has (Resumable (BaseError (EvalError term address value))) sig m
            , Has (Resumable (BaseError ResolutionError)) sig m
            , Has (State (Heap address address value)) sig m
            , Has (State (ScopeGraph address)) sig m
            , Has (Abstract.String value) sig m
            , Has (Reader (CurrentFrame address)) sig m
            , Has (Reader (CurrentScope address)) sig m
            , Has (State Span) sig m
            , Has (Unit value) sig m
            , Has (While value) sig m
            , Has Fresh sig m
            , Has Trace sig m
            , Ord address
            , Recursive term
            , Show address
            )
         => Open (term -> Evaluator term address value m value)
         -> term -> Evaluator term address value m value
-- NB: We use a lazy pattern match for the lambda’s argument to postpone evaluating the pair until eval/ref is called.
evalTerm perTerm = fst (fix (\ ~(ev, re) -> (perTerm (eval ev re . project), ref ev re . project)))
