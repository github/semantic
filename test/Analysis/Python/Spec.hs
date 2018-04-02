{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Analysis.Python.Spec (spec) where

import Data.Abstract.Value
import Data.Map

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evaluates Python" $ do
    it "imports" $ do
      res <- snd <$> evaluate "main.py"
      environment res `shouldBe` [ ("a", addr 0)
                                 , ("b", addr 2)
                                 ]

      heapLookup (Address (Precise 0)) (heap res) `shouldBe` ns "a" [ ("foo", addr 1) ]
      heapLookup (Address (Precise 2)) (heap res) `shouldBe` ns "b" [ ("c", addr 3) ]
      heapLookup (Address (Precise 3)) (heap res) `shouldBe` ns "c" [ ("baz", addr 4) ]

    it "imports with aliases" $ do
      env <- environment . snd <$> evaluate "main1.py"
      env `shouldBe` [ ("b", addr 0)
                     , ("e", addr 2)
                     ]

    it "imports using 'from' syntax" $ do
      env <- environment . snd <$> evaluate "main2.py"
      env `shouldBe` [ ("foo", addr 0)
                     , ("bar", addr 1)
                     ]

    it "subclasses" $ do
      v <- fst <$> evaluate "subclass.py"
      v `shouldBe` Right (Right (Right (Right (Right (injValue (String "\"bar\""))))))

    it "handles multiple inheritance left-to-right" $ do
      v <- fst <$> evaluate "multiple_inheritance.py"
      v `shouldBe` Right (Right (Right (Right (Right (injValue (String "\"foo!\""))))))

  where
    ns n = Just . Latest . Just . injValue . Namespace n
    addr = Address . Precise
    fixtures = "test/fixtures/python/analysis/"
    evaluate entry = evalPythonProject (fixtures <> entry)
