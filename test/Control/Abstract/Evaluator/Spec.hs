{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Control.Abstract.Evaluator.Spec
( spec
) where

import           Control.Abstract as Abstract
import qualified Control.Abstract.Heap as Heap
import           Control.Effect.Lift
import           Data.Abstract.Address.Precise as Precise
import           Data.Abstract.BaseError
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import qualified Data.Abstract.Number as Number
import           Data.Abstract.Package
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import           Data.Abstract.Value.Concrete as Value
import qualified Data.Language as Language
import qualified Data.Map.Strict as Map
import           Data.Sum
import           SpecHelpers hiding (reassociate)
import           System.IO.Unsafe (unsafePerformIO)

spec :: Spec
spec = do
  it "constructs integers" $ do
    (_, (_, (_, expected))) <- evaluate (integer 123)
    expected `shouldBe` Right (Value.Integer (Number.Integer 123))

  it "calls functions" $ do
    (_, (_, (_, expected))) <- evaluate . withLexicalScopeAndFrame $ do
      currentScope' <- currentScope
      let lexicalEdges = Map.singleton Lexical [ currentScope' ]
          x =  SpecHelpers.name "x"
      associatedScope <- newScope lexicalEdges
      declare (ScopeGraph.Declaration "identity") Default Public lowerBound ScopeGraph.Function (Just associatedScope)
      withScope associatedScope $ do
        declare (Declaration x) Default Public lowerBound ScopeGraph.RequiredParameter Nothing
      identity <- function "identity" [ x ]
        (SpecEff (Heap.lookupSlot (ScopeGraph.Declaration (SpecHelpers.name "x")) >>= deref)) associatedScope
      val <- integer 123
      call identity [val]
    expected `shouldBe` Right (Value.Integer (Number.Integer 123))

evaluate
  = runM
  . runTraceByIgnoring
  . runState (lowerBound @(ScopeGraph Precise))
  . runState (lowerBound @(Heap Precise Precise Val))
  . runFresh
  . runReader (PackageInfo (SpecHelpers.name "test") mempty)
  . runReader (ModuleInfo "test/Control/Abstract/Evaluator/Spec.hs" Language.Haskell mempty)
  . evalState (lowerBound @Span)
  . runReader (lowerBound @Span)
  . runEvaluator
  . runAllocator
  . evalModule
  where
    evalModule action = do
      scopeAddress <- newScope mempty
      frameAddress <- newFrame scopeAddress mempty
      val <- raiseHandler (runReader (CurrentScope scopeAddress))
        . raiseHandler (runReader (CurrentFrame frameAddress))
        . fmap reassociate
        . runScopeError
        . runHeapError
        . runValueError
        . runAddressError
        . runEvalError
        . runDeref @SpecEff
        . runAllocator
        . runReturn
        . runLoopControl
        . runNumeric
        . runBoolean
        . runFunction runSpecEff
        $ action
      pure ((scopeAddress, frameAddress), val)

reassociate :: Either (SomeError exc1) (Either (SomeError exc2) (Either (SomeError exc3) (Either (SomeError exc4) (Either (SomeError exc5) result)))) -> Either (SomeError (Sum '[exc5, exc4, exc3, exc2, exc1])) result
reassociate = mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . Right

type Val = Value SpecEff Precise
newtype SpecEff = SpecEff
  { runSpecEff :: Evaluator SpecEff Precise Val (FunctionC SpecEff Precise Val
                 (BooleanC Val
                 (NumericC Val
                 (ErrorC (LoopControl Val)
                 (ErrorC (Return Val)
                 (AllocatorC Precise
                 (DerefC Precise Val
                 (ResumableC (BaseError (EvalError SpecEff Precise Val))
                 (ResumableC (BaseError (AddressError Precise Val))
                 (ResumableC (BaseError (ValueError SpecEff Precise))
                 (ResumableC (BaseError (HeapError Precise))
                 (ResumableC (BaseError (ScopeError Precise))
                 (ReaderC (CurrentFrame Precise)
                 (ReaderC (CurrentScope Precise)
                 (AllocatorC Precise
                 (ReaderC Span
                 (StateC Span
                 (ReaderC ModuleInfo
                 (ReaderC PackageInfo
                 (FreshC
                 (StateC (Heap Precise Precise Val)
                 (StateC (ScopeGraph Precise)
                 (TraceByIgnoringC
                 (LiftC IO))))))))))))))))))))))))
                 Val
  }

instance Eq SpecEff where _ == _ = True
instance Show SpecEff where show _ = "_"
instance FreeVariables SpecEff where freeVariables _ = lowerBound

instance Declarations SpecEff where
  declaredName eff =
    case unsafePerformIO (evaluate (runSpecEff eff)) of
      (_, (_, (_, Right (Value.String text)))) -> Just (SpecHelpers.name text)
      _                                        -> error "declaredName for SpecEff should return an RVal"
