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
      identity <- function Nothing [name "x"] (coerce (variable (name "x")))
      recv <- box unit
      addr <- box (integer 123)
      call identity recv [addr]
    expected `shouldBe` Right (Value.Integer (Number.Integer 123))

evaluate
  = runM
  . runIgnoringTrace
  . runState (lowerBound @(Heap Precise Val))
  . runFresh 0
  . runReader (PackageInfo (name "test") mempty)
  . runReader (ModuleInfo "test/Control/Abstract/Evaluator/Spec.hs")
  . runReader (lowerBound @Span)
  . fmap reassociate
  . runValueError
  . runEnvironmentError
  . runAddressError
  . Precise.runDeref @_ @_ @Val
  . Precise.runAllocator
  . (>>= deref . snd)
  . runEnv lowerBound
  . runReturn
  . runLoopControl
  . Value.runBoolean
  . Value.runFunction coerce

reassociate :: Either (SomeExc exc1) (Either (SomeExc exc2) (Either (SomeExc exc3) result)) -> Either (SomeExc (Sum '[exc3, exc2, exc1])) result
reassociate = mergeExcs . mergeExcs . mergeExcs . Right

type Val = Value SpecEff Precise
newtype SpecEff = SpecEff
  { runSpecEff :: Eff '[ Function SpecEff Precise Val
                       , Boolean Val
                       , Exc (LoopControl Precise)
                       , Exc (Return Precise)
                       , Env Precise
                       , Allocator Precise
                       , Deref Val
                       , Resumable (BaseError (AddressError Precise Val))
                       , Resumable (BaseError (EnvironmentError Precise))
                       , Resumable (BaseError (ValueError SpecEff Precise))
                       , Reader Span
                       , Reader ModuleInfo
                       , Reader PackageInfo
                       , Fresh
                       , State (Heap Precise Val)
                       , Trace
                       , Lift IO
                       ] Precise
  }

instance Eq SpecEff where _ == _ = True
instance Show SpecEff where show _ = "_"
instance FreeVariables SpecEff where freeVariables _ = lowerBound
