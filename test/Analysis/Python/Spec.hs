{-# LANGUAGE TypeApplications #-}
module Analysis.Python.Spec (spec) where

import Data.Abstract.Value
import Data.Map

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evalutes Python" $ do
    it "imports" $ do
      env <- evaluate "main.py"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["a", "foo"], addr 0)
            , (qualifiedName ["b", "c", "baz"], addr 1)
            ]
      env `shouldBe` expectedEnv

    it "imports with aliases" $ do
      env <- evaluate "main1.py"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["b", "foo"], addr 0)
            , (qualifiedName ["e", "baz"], addr 1)
            ]
      env `shouldBe` expectedEnv

    it "imports using 'from' syntax" $ do
      env <- evaluate "main2.py"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["foo"], addr 0)
            , (qualifiedName ["bar"], addr 1)
            ]
      env `shouldBe` expectedEnv

  where
    addr = Address . Precise
    fixtures = "test/fixtures/python/analysis/"
    evaluate entry = snd . fst . fst . fst <$>
      evaluateFiles @(Value Precise) pythonParser
      [ fixtures <> entry
      , fixtures <> "a.py"
      , fixtures <> "b/c.py"
      ]
