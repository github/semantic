module Data.Abstract.Name.Spec where

import SpecHelpers

import Data.Abstract.Name

spec :: Spec
spec = describe "Data.Abstract.Name" $
  it "should format anonymous names correctly" $ do
    show (nameI 0)  `shouldBe` "\"_a\""
    show (nameI 26) `shouldBe` "\"_aʹ\""
