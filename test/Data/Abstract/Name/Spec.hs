module Data.Abstract.Name.Spec (spec) where

import SpecHelpers

spec :: Spec
spec = describe "Data.Abstract.Name" $
  it "should format anonymous names correctly" $ do
    show (nameI 0)  `shouldBe` "\"_a\""
    show (nameI 26) `shouldBe` "\"_aʹ\""
