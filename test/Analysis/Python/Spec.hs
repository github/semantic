{-# LANGUAGE DataKinds, ImplicitParams, OverloadedStrings, TypeApplications #-}
{-# OPTIONS_GHC -O0 #-}
module Analysis.Python.Spec (spec) where

import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Value.Concrete
import qualified Data.Language as Language
import qualified Language.Python.Term as Python
import           Source.Loc
import           SpecHelpers


spec :: (?session :: TaskSession) => Spec
spec = do
  describe "Python" $ do
    it "imports" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main.py", "a.py", "b/__init__.py", "b/c.py"]
      case ModuleTable.lookup "main.py" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) -> do
          SpecHelpers.lookupDeclaration "a" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          SpecHelpers.lookupDeclaration "b" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust

          fromJust (SpecHelpers.lookupMembers "a" Import scopeAndFrame heap scopeGraph) `shouldContain` [ "foo" ]
          fromJust (SpecHelpers.lookupMembers "b" Import scopeAndFrame heap scopeGraph) `shouldContain` ["c"]
          -- (derefQName heap ("b" :| ["c"]) env >>= deNamespace heap) `shouldBe` Just ("c", ["baz"])
        other -> expectationFailure (show other)

    it "imports with aliases" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main1.py", "a.py", "b/__init__.py", "b/c.py"]
      case ModuleTable.lookup "main1.py" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) -> do
          SpecHelpers.lookupDeclaration "b" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          SpecHelpers.lookupDeclaration "e" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
        other -> expectationFailure (show other)

    it "imports using from syntax" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main2.py", "a.py", "b/__init__.py", "b/c.py"]
      case ModuleTable.lookup "main2.py" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) -> do
          SpecHelpers.lookupDeclaration "bar" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          SpecHelpers.lookupDeclaration "foo" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust

          -- TODO: Enable when we constrain edge paths with path predicates
          -- () <$ SpecHelpers.lookupDeclaration "baz" heap scopeGraph `shouldBe` Nothing
        other -> expectationFailure (show other)

    it "imports with relative syntax" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main3.py", "c/__init__.py", "c/utils.py"]
      case ModuleTable.lookup "main3.py" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) -> do
          SpecHelpers.lookupDeclaration "utils" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          -- (lookupDeclaration "utils" heap >>= deNamespace heap) `shouldBe` Just ("utils", ["to_s"])
        other -> expectationFailure (show other)

    it "subclasses" $ do
      (scopeGraph, (heap, res)) <- evaluate ["subclass.py"]
      case ModuleTable.lookup "subclass.py" <$> res of
        Right (Just (Module _ (scopeAndFrame, value))) -> do
          SpecHelpers.lookupDeclaration "Foo" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          SpecHelpers.lookupDeclaration "Bar" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          SpecHelpers.lookupMembers "Bar" Superclass scopeAndFrame heap scopeGraph `shouldBe` Just [ "dang" ]
          value `shouldBe` String "\"bar\""
        other -> expectationFailure (show other)

    it "handles multiple inheritance left-to-right" $ do
      (scopeGraph, (heap, res)) <- evaluate ["multiple_inheritance.py"]
      case ModuleTable.lookup "multiple_inheritance.py" <$> res of
        Right (Just (Module _ (scopeAndFrame, value))) -> do
          SpecHelpers.lookupMembers "Baz" Superclass scopeAndFrame heap scopeGraph `shouldBe` Just [ "dang" ]
          value `shouldBe` String "\"bar!\""
        other -> expectationFailure (show other)

  where
    fixtures = "test/fixtures/python/analysis/"
    evaluate = evaluateProject @'Language.Python @(Python.Term Loc) ?session Proxy . map (fixtures <>)
