module Data.Abstract.Name.Spec where

import SpecHelpers

import Data.Abstract.Name
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec = testGroup "Data.Abstract.Name"
  [ testCase "should format anonymous names correctly" $ do
    show (nameI 0)  @?= "\"_a\""
    show (nameI 26) @?= "\"_aʹ\""
  ]
