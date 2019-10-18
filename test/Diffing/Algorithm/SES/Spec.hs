module Diffing.Algorithm.SES.Spec (spec) where

import Data.These
import Diffing.Algorithm.SES
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = do
  describe "ses" $ do
    prop "returns equal lists in These" $
      \ as -> (ses (==) as as :: [Edit Char Char]) `shouldBe` zipWith Compare as as

    prop "returns deletions in This" $
      \ as -> (ses (==) as [] :: [Edit Char Char]) `shouldBe` fmap Delete as

    prop "returns insertions in That" $
      \ bs -> (ses (==) [] bs :: [Edit Char Char]) `shouldBe` fmap Insert bs

    prop "returns all elements individually for disjoint inputs" $
      \ as bs -> length (ses (==) ((,) 0 <$> as :: [(Int, Char)]) ((,) 1 <$> bs :: [(Int, Char)])) `shouldBe` length as + length bs

    prop "is lossless w.r.t. both input elements & ordering" $
      \ as bs -> foldr (\ each (as, bs) -> these (flip (,) bs. (:as)) ((,) as . (:bs)) (\ a b -> (a:as, b:bs)) (toThese each)) ([], []) (ses (==) as bs :: [Edit Char Char]) `shouldBe` (as, bs)
