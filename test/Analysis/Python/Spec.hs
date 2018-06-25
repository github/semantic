module Analysis.Python.Spec (spec) where

import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable (EvalError(..))
import Data.Abstract.Value
import qualified Language.Python.Assignment as Python
import qualified Data.Language as Language

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "Python" $ do
    it "imports" $ do
      ((res@(~(Right [(_, env)])), heap), _) <- evaluate ["main.py", "a.py", "b/__init__.py", "b/c.py"]
      fmap (() <$) res `shouldBe` Right [()]
      Env.names env `shouldContain` [ "a", "b" ]

      (derefQName heap ("a" :| [])    env >>= deNamespace) `shouldBe` Just ("a", ["foo"])
      (derefQName heap ("b" :| [])    env >>= deNamespace) `shouldBe` Just ("b", ["c"])
      (derefQName heap ("b" :| ["c"]) env >>= deNamespace) `shouldBe` Just ("c", ["baz"])

    it "imports with aliases" $ do
      ((res@(~(Right [(_, env)])), _), _) <- evaluate ["main1.py", "a.py", "b/__init__.py", "b/c.py"]
      fmap (() <$) res `shouldBe` Right [()]
      Env.names env `shouldContain` [ "b", "e" ]

    it "imports using 'from' syntax" $ do
      ((res@(~(Right [(_, env)])), _), _) <- evaluate ["main2.py", "a.py", "b/__init__.py", "b/c.py"]
      fmap (() <$) res `shouldBe` Right [()]
      Env.names env `shouldContain` [ "bar", "foo" ]

    it "imports with relative syntax" $ do
      ((res@(~(Right [(_, env)])), heap), _) <- evaluate ["main3.py", "c/__init__.py", "c/utils.py"]
      fmap (() <$) res `shouldBe` Right [()]
      Env.names env `shouldContain` [ "utils" ]
      (derefQName heap ("utils" :| []) env >>= deNamespace) `shouldBe` Just ("utils", ["to_s"])

    it "subclasses" $ do
      ((res, _), _) <- evaluate ["subclass.py"]
      fmap fst <$> res `shouldBe` Right [String "\"bar\""]

    it "handles multiple inheritance left-to-right" $ do
      ((res, _), _) <- evaluate ["multiple_inheritance.py"]
      fmap fst <$> res `shouldBe` Right [String "\"foo!\""]

  where
    ns n = Just . Latest . Last . Just . Namespace n
    fixtures = "test/fixtures/python/analysis/"
    evaluate = evalPythonProject . map (fixtures <>)
    evalPythonProject = testEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Python) pythonParser Language.Python
