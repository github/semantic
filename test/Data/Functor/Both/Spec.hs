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
