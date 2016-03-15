module Data.Functor.Both.Spec (spec) where

import Data.Coalescent
import Data.Functor.Both
import Line
import Test.Hspec

spec :: Spec
spec = do
  describe "Coalescent" $ do
    it "should coalesce when both sides coalesce" $
      (both (Line [True]) (Line [True]) `coalesce` both (Line [True]) (Line [True]) :: [Both (Line Bool)]) `shouldBe` [both (Line [True, True]) (Line [True, True])]
