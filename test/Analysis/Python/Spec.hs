{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Analysis.Python.Spec (spec) where

import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable (EvalError(..))
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
      Env.names (environment res) `shouldBe` [ "a", "b", "print" ]

      (derefQName (heap res) ("a" :| [])    (environment res) >>= deNamespace) `shouldBe` Just ("a", ["foo"])
      (derefQName (heap res) ("b" :| [])    (environment res) >>= deNamespace) `shouldBe` Just ("b", ["c"])
      (derefQName (heap res) ("b" :| ["c"]) (environment res) >>= deNamespace) `shouldBe` Just ("c", ["baz"])

    it "imports with aliases" $ do
      env <- environment . snd <$> evaluate "main1.py"
      Env.names env `shouldBe` [ "b", "e", "print" ]

    it "imports using 'from' syntax" $ do
      env <- environment . snd <$> evaluate "main2.py"
      Env.names env `shouldBe` [ "bar", "foo", "print" ]

    it "subclasses" $ do
      v <- fst <$> evaluate "subclass.py"
      v `shouldBe` Right [injValue (String "\"bar\"")]

    it "handles multiple inheritance left-to-right" $ do
      v <- fst <$> evaluate "multiple_inheritance.py"
      v `shouldBe` Right [injValue (String "\"foo!\"")]

  where
    ns n = Just . Latest . Just . injValue . Namespace n
    addr = Address . Precise
    fixtures = "test/fixtures/python/analysis/"
    evaluate entry = evalPythonProject (fixtures <> entry)
    evalPythonProject path = testEvaluating <$> evaluateProject pythonParser Language.Python pythonPrelude path
