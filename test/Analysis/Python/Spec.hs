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
      (_, res) <- evaluate ["main.py", "a.py", "b/__init__.py", "b/c.py"]
      case ModuleTable.lookup "main.py" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, value)) :| [])) -> do
          Env.names env `shouldContain` [ "a", "b" ]

          (lookupDeclaration "a" heap >>= deNamespace heap) `shouldBe` Just ("a", ["foo"])
          (lookupDeclaration "b" heap >>= deNamespace heap) `shouldBe` Just ("b", ["c"])
          undefined
          -- (derefQName heap ("b" :| ["c"]) env >>= deNamespace heap) `shouldBe` Just ("c", ["baz"])
        other -> expectationFailure (show other)

    it "imports with aliases" $ do
      (_, (_, res)) <- evaluate ["main1.py", "a.py", "b/__init__.py", "b/c.py"]
      case ModuleTable.lookup "main1.py" <$> res of
        Right (Just (Module _ (_, (env, addr)) :| [])) -> Env.names env `shouldContain` [ "b", "e" ]
        other -> expectationFailure (show other)

    it "imports using 'from' syntax" $ do
      (_, (_, res)) <- evaluate ["main2.py", "a.py", "b/__init__.py", "b/c.py"]
      case ModuleTable.lookup "main2.py" <$> res of
        Right (Just (Module _ (_, (env, addr)) :| [])) -> Env.names env `shouldContain` [ "bar", "foo" ]
        other -> expectationFailure (show other)

    it "imports with relative syntax" $ do
      (_, (heap, res)) <- evaluate ["main3.py", "c/__init__.py", "c/utils.py"]
      case ModuleTable.lookup "main3.py" <$> res of
        Right (Just (Module _ (_, (env, addr)) :| [])) -> do
          Env.names env `shouldContain` [ "utils" ]
          (lookupDeclaration "utils" heap >>= deNamespace heap) `shouldBe` Just ("utils", ["to_s"])
        other -> expectationFailure (show other)

    it "subclasses" $ do
      (_, (heap, res)) <- evaluate ["subclass.py"]
      case ModuleTable.lookup "subclass.py" <$> res of
        Right (Just (Module _ (_, (env, addr)) :| [])) -> heapLookupAll addr heap `shouldBe` Just [String "\"bar\""]
        other -> expectationFailure (show other)

    it "handles multiple inheritance left-to-right" $ do
      (_, (heap, res)) <- evaluate ["multiple_inheritance.py"]
      case ModuleTable.lookup "multiple_inheritance.py" <$> res of
        Right (Just (Module _ (_, (env, addr)) :| [])) -> heapLookupAll addr heap `shouldBe` Just [String "\"foo!\""]
        other -> expectationFailure (show other)

  where
    fixtures = "test/fixtures/python/analysis/"
    evaluate = evalPythonProject . map (fixtures <>)
    evalPythonProject = testEvaluating <=< evaluateProject' config (Proxy :: Proxy 'Language.Python) pythonParser
