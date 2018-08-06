{-# LANGUAGE TypeOperators #-}
module Control.Abstract.Evaluator.Spec
( spec
, SpecEff(..)
) where

import Control.Abstract
import Data.Abstract.ErrorContext
import Data.Abstract.Module
import qualified Data.Abstract.Number as Number
import Data.Abstract.Package
import Data.Abstract.Value.Concrete as Value
import Data.Algebra
import Data.Bifunctor (first)
import Data.Coerce
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
      identity <- function [name "x"] lowerBound (variable (name "x"))
      addr <- box (integer 123)
      call identity [addr]
    expected `shouldBe` Right (Value.Integer (Number.Integer 123))

evaluate
  = runM
  . runState (lowerBound @(Heap Precise Latest Val))
  . runFresh 0
  . runReader (PackageInfo (name "test") mempty)
  . runReader (ModuleInfo "test/Control/Abstract/Evaluator/Spec.hs")
  . runReader (lowerBound @Span)
  . fmap reassociate
  . runValueError
  . runEnvironmentError
  . runAddressError
  . runDeref
  . runAllocator @Precise @_ @Val
  . (>>= deref . snd)
  . runEnv lowerBound
  . runReturn
  . runLoopControl
  . Value.runFunction coerce coerce

reassociate :: Either (SomeExc exc1) (Either (SomeExc exc2) (Either (SomeExc exc3) result)) -> Either (SomeExc (Sum '[exc3, exc2, exc1])) result
reassociate = mergeExcs . mergeExcs . mergeExcs . Right

type Val = Value Precise SpecEff
newtype SpecEff a = SpecEff
  { runSpecEff :: Eff '[ Function Precise Val
                       , Exc (LoopControl Precise)
                       , Exc (Return Precise)
                       , Env Precise
                       , Allocator Precise Val
                       , Deref Precise Val
                       , Resumable (BaseError (AddressError Precise Val))
                       , Resumable (BaseError (EnvironmentError Precise))
                       , Resumable (BaseError (ValueError Precise SpecEff))
                       , Reader Span
                       , Reader ModuleInfo
                       , Reader PackageInfo
                       , Fresh
                       , State (Heap Precise Latest Val)
                       , Lift IO
                       ] a
  }
