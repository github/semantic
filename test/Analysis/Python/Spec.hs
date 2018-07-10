module Analysis.Python.Spec (spec) where

import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable (EvalError(..))
import qualified Data.Abstract.ModuleTable as ModuleTable
import Data.Abstract.Value.Concrete
import qualified Language.Python.Assignment as Python
import qualified Data.Language as Language

import SpecHelpers


spec :: TaskConfig -> Spec
spec config = parallel $ do
  describe "Python" $ do
    it "imports" $ do
      (_, (heap, res)) <- evaluate ["main.py", "a.py", "b/__init__.py", "b/c.py"]
      case ModuleTable.lookup "main.py" <$> res of
        Right (Just (Module _ (env, addr) :| [])) -> do
          Env.names env `shouldContain` [ "a", "b" ]

          (derefQName heap ("a" :| [])    env >>= deNamespace) `shouldBe` Just ("a", ["foo"])
          (derefQName heap ("b" :| [])    env >>= deNamespace) `shouldBe` Just ("b", ["c"])
          (derefQName heap ("b" :| ["c"]) env >>= deNamespace) `shouldBe` Just ("c", ["baz"])
        other -> expectationFailure (show other)

    it "imports with aliases" $ do
      (_, (_, res)) <- evaluate ["main1.py", "a.py", "b/__init__.py", "b/c.py"]
      case ModuleTable.lookup "main1.py" <$> res of
        Right (Just (Module _ (env, addr) :| [])) -> Env.names env `shouldContain` [ "b", "e" ]
        other -> expectationFailure (show other)

    it "imports using 'from' syntax" $ do
      (_, (_, res)) <- evaluate ["main2.py", "a.py", "b/__init__.py", "b/c.py"]
      case ModuleTable.lookup "main2.py" <$> res of
        Right (Just (Module _ (env, addr) :| [])) -> Env.names env `shouldContain` [ "bar", "foo" ]
        other -> expectationFailure (show other)

    it "imports with relative syntax" $ do
      (_, (heap, res)) <- evaluate ["main3.py", "c/__init__.py", "c/utils.py"]
      case ModuleTable.lookup "main3.py" <$> res of
        Right (Just (Module _ (env, addr) :| [])) -> do
          Env.names env `shouldContain` [ "utils" ]
          (derefQName heap ("utils" :| []) env >>= deNamespace) `shouldBe` Just ("utils", ["to_s"])
        other -> expectationFailure (show other)

    it "subclasses" $ do
      (_, (heap, res)) <- evaluate ["subclass.py"]
      case ModuleTable.lookup "subclass.py" <$> res of
        Right (Just (Module _ (env, addr) :| [])) -> heapLookupAll addr heap `shouldBe` Just [String "\"bar\""]
        other -> expectationFailure (show other)

    it "handles multiple inheritance left-to-right" $ do
      (_, (heap, res)) <- evaluate ["multiple_inheritance.py"]
      case ModuleTable.lookup "multiple_inheritance.py" <$> res of
        Right (Just (Module _ (env, addr) :| [])) -> heapLookupAll addr heap `shouldBe` Just [String "\"foo!\""]
        other -> expectationFailure (show other)

  where
    ns n = Just . Latest . Last . Just . Namespace n
    fixtures = "test/fixtures/python/analysis/"
    evaluate = evalPythonProject . map (fixtures <>)
    evalPythonProject = testEvaluating <=< evaluateProject' config (Proxy :: Proxy 'Language.Python) pythonParser Language.Python
