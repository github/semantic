module Data.Functor.Both.Spec (spec) where

import Data.Coalescent
import Data.Functor.Both
import Line
import Test.Hspec

spec :: Spec
spec = do
  describe "Coalescent" $ do
    it "should coalesce when both sides coalesce" $
      (pure (Line [True]) `coalesce` pure (Line [True]) :: [Both (Line Bool)]) `shouldBe` [pure (Line [True, True])]

    it "should not coalesce when neither side coalesces" $
      (pure (Closed [True]) `coalesce` pure (Line [True]) :: [Both (Line Bool)]) `shouldBe` [pure (Closed [True]), pure (Line [True])]
