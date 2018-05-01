{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Analysis.Python.Spec (spec) where

import Data.Abstract.Evaluatable (EvalError(..), interpret)
import Data.Abstract.Value
import Data.Map
import qualified Language.Python.Assignment as Python
import qualified Data.Language as Language

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evaluates Python" $ do
    it "imports" $ do
      res <- snd <$> evaluate "main.py"
      environment res `shouldBe` [ ("print", addr 0)
                                 , ("a", addr 1)
                                 , ("b", addr 3)
                                 ]

      heapLookup (Address (Precise 1)) (heap res) `shouldBe` ns "a" [ ("foo", addr 2) ]
      heapLookup (Address (Precise 3)) (heap res) `shouldBe` ns "b" [ ("c", addr 4) ]
      heapLookup (Address (Precise 4)) (heap res) `shouldBe` ns "c" [ ("baz", addr 5) ]

    it "imports with aliases" $ do
      env <- environment . snd <$> evaluate "main1.py"
      env `shouldBe` [ ("print", addr 0)
                     , ("b", addr 1)
                     , ("e", addr 3)
                     ]

    it "imports using 'from' syntax" $ do
      env <- environment . snd <$> evaluate "main2.py"
      env `shouldBe` [ ("print", addr 0)
                     , ("foo", addr 1)
                     , ("bar", addr 2)
                     ]

    it "subclasses" $ do
      v <- fst <$> evaluate "subclass.py"
      v `shouldBe` Right (Right (Right (Right (Right (Right (Right (pure (injValue (String "\"bar\"")))))))))

    it "handles multiple inheritance left-to-right" $ do
      v <- fst <$> evaluate "multiple_inheritance.py"
      v `shouldBe` Right (Right (Right (Right (Right (Right (Right (pure (injValue (String "\"foo!\"")))))))))

  where
    ns n = Just . Latest . Just . injValue . Namespace n
    addr = Address . Precise
    fixtures = "test/fixtures/python/analysis/"
    evaluate entry = evalPythonProject (fixtures <> entry)
    evalPythonProject path = interpret @(TestEvaluating Python.Term) <$> evaluateProject pythonParser Language.Python pythonPrelude path
