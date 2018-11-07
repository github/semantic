{-# LANGUAGE TypeOperators #-}
module Control.Abstract.Evaluator.Spec
( spec
) where

import Control.Abstract
import Data.Abstract.Address.Precise as Precise
import Data.Abstract.BaseError
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Module
import qualified Data.Abstract.Number as Number
import Data.Abstract.Package
import Data.Abstract.Value.Concrete as Value
import Data.Algebra
import Data.Bifunctor (first)
import Data.Functor.Const
import Data.Sum
import SpecHelpers hiding (reassociate)

spec :: Spec
spec = parallel $ do
  it "constructs integers" $ do
    (_, expected) <- evaluate (box (integer 123))
    expected `shouldBe` Right (Value.Integer (Number.Integer 123))

  it "calls functions" $ do
    (_, expected) <- evaluate $ do
      identity <- function Nothing [name "x"] (SpecEff (variable (name "x")))
      recv <- box unit
      addr <- box (integer 123)
      call identity recv [addr]
    expected `shouldBe` Right (Value.Integer (Number.Integer 123))

evaluate
  = runM
  . runTraceByIgnoring
  . runState (lowerBound @(Heap Precise Val))
  . runFresh
  . runReader (PackageInfo (name "test") mempty)
  . runReader (ModuleInfo "test/Control/Abstract/Evaluator/Spec.hs")
  . runReader (lowerBound @Span)
  . runEvaluator
  . fmap reassociate
  . runValueError
  . runAddressError
  . runDeref @Val
  . runAllocator
  . (>>= deref . snd)
  . runEnv lowerBound
  . runReturn
  . runLoopControl
  . runBoolean
  . runFunction runSpecEff

reassociate :: Either (SomeError exc1) (Either (SomeError exc2) (Either (SomeError exc3) result)) -> Either (SomeError (Sum '[exc3, exc2, exc1])) result
reassociate = mergeErrors . mergeErrors . mergeErrors . Right

type Val = Value SpecEff Precise
newtype SpecEff = SpecEff
  { runSpecEff :: Evaluator SpecEff Precise Val (FunctionC SpecEff Precise Val
                 (Eff (BooleanC Val
                 (Eff (ErrorC (LoopControl Precise)
                 (Eff (ErrorC (Return Precise)
                 (Eff (EnvC Precise
                 (Eff (AllocatorC Precise
                 (Eff (DerefC Precise Val
                 (Eff (ResumableC (BaseError (AddressError Precise Val))
                 (Eff (ResumableC (BaseError (ValueError SpecEff Precise))
                 (Eff (ReaderC Span
                 (Eff (ReaderC ModuleInfo
                 (Eff (ReaderC PackageInfo
                 (Eff (FreshC
                 (Eff (StateC (Heap Precise Val)
                 (Eff (TraceByIgnoringC
                 (Eff (LiftC IO)))))))))))))))))))))))))))))))
                 Precise
  }

instance Eq SpecEff where _ == _ = True
instance Show SpecEff where show _ = "_"
instance FreeVariables SpecEff where freeVariables _ = lowerBound
