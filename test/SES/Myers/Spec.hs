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
      \ as -> runMyers (==) (makeEditGraph as as :: EditGraph Char Char) ses `shouldBe` zipWith These as as

    prop "returns deletions in This" $
      \ as -> runMyers (==) (makeEditGraph as [] :: EditGraph Char Char) ses `shouldBe` fmap This as

    prop "returns insertions in That" $
      \ bs -> runMyers (==) (makeEditGraph [] bs :: EditGraph Char Char) ses `shouldBe` fmap That bs

    prop "returns all elements individually for disjoint inputs" $
      \ as bs -> length (runMyers (==) (makeEditGraph ((,) 0 <$> as :: [(Int, Char)]) ((,) 1 <$> bs :: [(Int, Char)])) ses) `shouldBe` length as + length bs

    prop "is lossless w.r.t. both input elements & ordering" $
      \ as bs -> foldr (\ each (as, bs) -> these (flip (,) bs. (:as)) ((,) as . (:bs)) (\ a b -> (a:as, b:bs)) each) ([], []) (runMyers (==) (makeEditGraph as bs :: EditGraph Char Char) ses) `shouldBe` (as, bs)
