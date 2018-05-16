{-# LANGUAGE TypeOperators #-}
module Control.Abstract.Evaluator.Spec
( spec
) where

import Analysis.Abstract.Evaluating (evaluating)
import Control.Abstract
import Data.Abstract.Module
import qualified Data.Abstract.Number as Number
import Data.Abstract.Package
import Data.Abstract.Value as Value
import Data.Algebra
import Data.Bifunctor (first)
import Data.Functor.Const
import Data.Semilattice.Lower
import Data.Sum
import SpecHelpers hiding (reassociate)

spec :: Spec
spec = parallel $ do
  it "constructs integers" $ do
    (expected, _) <- evaluate (integer 123)
    expected `shouldBe` Right (injValue (Value.Integer (Number.Integer 123)))

  it "calls functions" $ do
    (expected, _) <- evaluate $ do
      identity <- closure [name "x"] lowerBound (variable (name "x"))
      call identity [integer 123]
    expected `shouldBe` Right (injValue (Value.Integer (Number.Integer 123)))

evaluate
  = runM
  . fmap (first reassociate)
  . evaluating @Precise @(Value Precise)
  . runReader (PackageInfo (name "test") Nothing)
  . runReader (ModuleInfo "test/Control/Abstract/Evaluator/Spec.hs")
  . Value.runValueError
  . runEnvironmentError
  . runAddressError
  . runReturn
  . runLoopControl
  . fmap fst
  . runState (Gotos lowerBound)
  . runGoto Gotos getGotos

newtype Gotos effects = Gotos { getGotos :: GotoTable (State (Gotos effects) ': effects) (Value Precise) }

reassociate :: Either Prelude.String (Either (SomeExc exc1) (Either (SomeExc exc2) (Either (SomeExc exc3) result))) -> Either (SomeExc (Sum '[Const Prelude.String, exc1, exc2, exc3])) result
reassociate (Left s) = Left (SomeExc (injectSum (Const s)))
reassociate (Right (Right (Right (Right a)))) = Right a
