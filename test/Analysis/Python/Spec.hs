{-# LANGUAGE OverloadedLists, TypeApplications #-}
module Analysis.Python.Spec (spec) where

import Data.Abstract.Value
import Data.Map

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evalutes Python" $ do
    it "imports" $ do
      env <- evaluate "main.py"
      let expectedEnv =
            [ (qualifiedName ["a", "foo"], addr 0)
            , (qualifiedName ["b", "c", "baz"], addr 1)
            ]
      env `shouldBe` expectedEnv

    it "imports with aliases" $ do
      env <- evaluate "main1.py"
      let expectedEnv =
            [ (qualifiedName ["b", "foo"], addr 0)
            , (qualifiedName ["e", "baz"], addr 1)
            ]
      env `shouldBe` expectedEnv

    it "imports using 'from' syntax" $ do
      env <- evaluate "main2.py"
      let expectedEnv =
            [ (qualifiedName ["foo"], addr 0)
            , (qualifiedName ["bar"], addr 1)
            ]
      env `shouldBe` expectedEnv

    it "subclasses" $ do
      res <- evaluate' "subclass.py"
      fst res `shouldBe` Right (injValue (String "\"bar\""))

    it "handles multiple inheritance left-to-right" $ do
      res <- evaluate' "multiple_inheritance.py"
      fst res `shouldBe` Right (injValue (String "\"foo!\""))

  where
    addr = Address . Precise
    fixtures = "test/fixtures/python/analysis/"
    evaluate entry = snd <$> evaluate' entry
    evaluate' entry = fst . fst . fst . fst <$>
      evaluateFiles pythonParser
      [ fixtures <> entry
      , fixtures <> "a.py"
      , fixtures <> "b/c.py"
      ]
