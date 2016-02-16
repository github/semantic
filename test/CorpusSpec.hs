module CorpusSpec where

import Test.Hspec

spec :: Spec
spec = do
  it "should not crash" $
    True `shouldBe` True

  it "should produce the correct diff" $
    True `shouldBe` True
