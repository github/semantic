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
      \ as -> (ses (==) as as :: EditGraph Char Char) `shouldBe` zipWith These as as

    prop "returns deletions in This" $
      \ as -> (ses (==) as [] :: EditGraph Char Char) ses `shouldBe` fmap This as

    prop "returns insertions in That" $
      \ bs -> (ses (==) [] bs :: EditGraph Char Char) ses `shouldBe` fmap That bs

    prop "returns all elements individually for disjoint inputs" $
      \ as bs -> length (ses (==) ((,) 0 <$> as :: [(Int, Char)]) ((,) 1 <$> bs :: [(Int, Char)])) `shouldBe` length as + length bs

    prop "is lossless w.r.t. both input elements & ordering" $
      \ as bs -> foldr (\ each (as, bs) -> these (flip (,) bs. (:as)) ((,) as . (:bs)) (\ a b -> (a:as, b:bs)) each) ([], []) (ses (==) as bs :: EditGraph Char Char) `shouldBe` (as, bs)
