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
      (_, (state, Right [(env, _)])) <- evaluate "main.py"
      Env.names env `shouldContain` [ "a", "b" ]

      (derefQName (heap state) ("a" :| [])    env >>= deNamespace) `shouldBe` Just ("a", ["foo"])
      (derefQName (heap state) ("b" :| [])    env >>= deNamespace) `shouldBe` Just ("b", ["c"])
      (derefQName (heap state) ("b" :| ["c"]) env >>= deNamespace) `shouldBe` Just ("c", ["baz"])

    it "imports with aliases" $ do
      (_, (_, Right [(env, _)])) <- evaluate "main1.py"
      Env.names env `shouldContain` [ "b", "e" ]

    it "imports using 'from' syntax" $ do
      (_, (_, Right [(env, _)])) <- evaluate "main2.py"
      Env.names env `shouldContain` [ "bar", "foo" ]

    it "imports with relative syntax" $ do
      (_, (state, Right [(env, _)])) <- evaluate "main3.py"
      Env.names env `shouldContain` [ "utils" ]
      (derefQName (heap state) ("utils" :| []) env >>= deNamespace) `shouldBe` Just ("utils", ["to_s"])

    it "subclasses" $ do
      (_, (_, res)) <- evaluate "subclass.py"
      fmap snd <$> res `shouldBe` Right [String "\"bar\""]

    it "handles multiple inheritance left-to-right" $ do
      (_, (_, res)) <- evaluate "multiple_inheritance.py"
      fmap snd <$> res `shouldBe` Right [String "\"foo!\""]

  where
    ns n = Just . Latest . Last . Just . Namespace n
    fixtures = "test/fixtures/python/analysis/"
    evaluate entry = evalPythonProject (fixtures <> entry)
    evalPythonProject path = testEvaluating <$> evaluateProject (Proxy :: Proxy 'Language.Python) pythonParser Language.Python path
