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
      ((_, state), _) <- evaluate "main.py"
      Env.names (environment state) `shouldBe` [ "a", "b", "print" ]

      (derefQName (heap state) ("a" :| [])    (environment state) >>= deNamespace) `shouldBe` Just ("a", ["foo"])
      (derefQName (heap state) ("b" :| [])    (environment state) >>= deNamespace) `shouldBe` Just ("b", ["c"])
      (derefQName (heap state) ("b" :| ["c"]) (environment state) >>= deNamespace) `shouldBe` Just ("c", ["baz"])

    it "imports with aliases" $ do
      env <- environment . snd . fst <$> evaluate "main1.py"
      Env.names env `shouldBe` [ "b", "e", "print" ]

    it "imports using 'from' syntax" $ do
      env <- environment . snd . fst <$> evaluate "main2.py"
      Env.names env `shouldBe` [ "bar", "foo", "print" ]

    it "subclasses" $ do
      ((res, _), _) <- evaluate "subclass.py"
      res `shouldBe` Right [injValue (String "\"bar\"")]

    it "handles multiple inheritance left-to-right" $ do
      ((res, _), _) <- evaluate "multiple_inheritance.py"
      res `shouldBe` Right [injValue (String "\"foo!\"")]

  where
    ns n = Just . Latest . Just . injValue . Namespace n
    addr = Address . Precise
    fixtures = "test/fixtures/python/analysis/"
    evaluate entry = evalPythonProject (fixtures <> entry)
    evalPythonProject path = testEvaluating <$> evaluateProject pythonParser Language.Python pythonPrelude path
