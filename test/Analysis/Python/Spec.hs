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
      ((Right [(_, env)], heap), _) <- evaluate "main.py"
      Env.names env `shouldContain` [ "a", "b" ]

      (derefQName heap ("a" :| [])    env >>= deNamespace) `shouldBe` Just ("a", ["foo"])
      (derefQName heap ("b" :| [])    env >>= deNamespace) `shouldBe` Just ("b", ["c"])
      (derefQName heap ("b" :| ["c"]) env >>= deNamespace) `shouldBe` Just ("c", ["baz"])

    it "imports with aliases" $ do
      ((Right [(_, env)], _), _) <- evaluate "main1.py"
      Env.names env `shouldContain` [ "b", "e" ]

    it "imports using 'from' syntax" $ do
      ((Right [(_, env)], _), _) <- evaluate "main2.py"
      Env.names env `shouldContain` [ "bar", "foo" ]

    it "imports with relative syntax" $ do
      ((Right [(_, env)], heap), _) <- evaluate "main3.py"
      Env.names env `shouldContain` [ "utils" ]
      (derefQName heap ("utils" :| []) env >>= deNamespace) `shouldBe` Just ("utils", ["to_s"])

    it "subclasses" $ do
      ((res, _), _) <- evaluate "subclass.py"
      fmap fst <$> res `shouldBe` Right [String "\"bar\""]

    it "handles multiple inheritance left-to-right" $ do
      ((res, _), _) <- evaluate "multiple_inheritance.py"
      fmap fst <$> res `shouldBe` Right [String "\"foo!\""]

  where
    ns n = Just . Latest . Last . Just . Namespace n
    fixtures = "test/fixtures/python/analysis/"
    evaluate entry = evalPythonProject (fixtures <> entry)
    evalPythonProject path = testEvaluating <$> evaluateProject (Proxy :: Proxy 'Language.Python) pythonParser Language.Python path
