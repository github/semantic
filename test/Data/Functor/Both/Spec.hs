module Data.Functor.Both.Spec (spec) where

import Data.Adjoined
import Data.Coalescent
import Data.Functor.Both
import Line
import Test.Hspec

spec :: Spec
spec = do
  describe "Coalescent" $ do
    it "should coalesce when both sides coalesce" $
      (pure (Line [True]) `coalesce` pure (Line [True]) :: Adjoined (Both (Line Bool))) `shouldBe` fromList [pure (Line [True, True])]

    it "should not coalesce when neither side coalesces" $
      (pure (Closed [True]) `coalesce` pure (Line [True]) :: Adjoined (Both (Line Bool))) `shouldBe` fromList [pure (Closed [True]), pure (Line [True])]

    it "should coalesce asymmetrically at left" $
      (both (Line [True]) (Closed [True]) `coalesce` pure (Line [True]) :: Adjoined (Both (Line Bool))) `shouldBe` fromList [both (Line [True, True]) (Closed [True]), both (Line []) (Line [True])]

    it "should coalesce asymmetrically at right" $
      (both (Closed [True]) (Line [True]) `coalesce` pure (Line [True]) :: Adjoined (Both (Line Bool))) `shouldBe` fromList [both (Closed [True]) (Line [True, True]), both (Line [True]) (Line [])]
