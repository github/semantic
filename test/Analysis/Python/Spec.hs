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
      Env.names (environment state) `shouldContain` [ "a", "b" ]

      (derefQName (heap state) ("a" :| [])    (environment state) >>= deNamespace) `shouldBe` Just ("a", ["foo"])
      (derefQName (heap state) ("b" :| [])    (environment state) >>= deNamespace) `shouldBe` Just ("b", ["c"])
      (derefQName (heap state) ("b" :| ["c"]) (environment state) >>= deNamespace) `shouldBe` Just ("c", ["baz"])

    it "imports with aliases" $ do
      env <- environment . snd . fst <$> evaluate "main1.py"
      Env.names env `shouldContain` [ "b", "e" ]

    it "imports using 'from' syntax" $ do
      env <- environment . snd . fst <$> evaluate "main2.py"
      Env.names env `shouldContain` [ "bar", "foo" ]

    it "imports with relative syntax" $ do
      ((_, state), _) <- evaluate "main3.py"
      Env.names (environment state) `shouldContain` [ "utils" ]
      (derefQName (heap state) ("utils" :| []) (environment state) >>= deNamespace) `shouldBe` Just ("utils", ["to_s"])

    it "subclasses" $ do
      ((res, _), _) <- evaluate "subclass.py"
      res `shouldBe` Right [String "\"bar\""]

    it "handles multiple inheritance left-to-right" $ do
      ((res, _), _) <- evaluate "multiple_inheritance.py"
      res `shouldBe` Right [String "\"foo!\""]

  where
    ns n = Just . Latest . Last . Just . Namespace n
    fixtures = "test/fixtures/python/analysis/"
    evaluate entry = evalPythonProject (fixtures <> entry)
    evalPythonProject path = testEvaluating <$> evaluateProject pythonParser Language.Python pythonPrelude path
