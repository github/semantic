{-# LANGUAGE OverloadedLists #-}
module Analysis.PHP.Spec (spec) where

import Data.Abstract.Value
import Data.Map

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evalutes PHP" $ do
    it "include and require" $ do
      env <- evaluate "main.php"
      let expectedEnv = [ (qualifiedName ["foo"], addr 0)
                        , (qualifiedName ["bar"], addr 1) ]
      env `shouldBe` expectedEnv

    it "include_once and require_once" $ do
      env <- evaluate "main_once.php"
      let expectedEnv = [ (qualifiedName ["foo"], addr 0)
                        , (qualifiedName ["bar"], addr 1) ]
      env `shouldBe` expectedEnv

  where
    addr = Address . Precise
    fixtures = "test/fixtures/php/analysis/"
    evaluate entry = snd . fst . fst . fst . fst <$>
      evaluateFiles phpParser
      [ fixtures <> entry
      , fixtures <> "foo.php"
      , fixtures <> "bar.php"
      ]
