module SES.Myers.Spec where

import Data.These
import Prologue
import SES.Myers
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = do
  describe "ses" $ do
    prop "returns equal lists in These" $
      \ as -> runMyers (==) (ses (makeEditGraph as as :: EditGraph Char Char)) `shouldBe` zipWith These as as

    prop "returns deletions in This" $
      \ as -> runMyers (==) (ses (makeEditGraph as [] :: EditGraph Char Char)) `shouldBe` fmap This as

    prop "returns insertions in That" $
      \ bs -> runMyers (==) (ses (makeEditGraph [] bs :: EditGraph Char Char)) `shouldBe` fmap That bs
