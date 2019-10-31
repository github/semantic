{-# LANGUAGE DataKinds, ImplicitParams, OverloadedStrings, TypeApplications #-}
{-# OPTIONS_GHC -O0 #-}
module Analysis.Go.Spec (spec) where

import qualified Data.Abstract.ModuleTable as ModuleTable
import qualified Data.Language as Language
import qualified Language.Go.Term as Go
import           Source.Loc
import           SpecHelpers


spec :: (?session :: TaskSession) => Spec
spec = do
  describe "Go" $ do
    it "imports and wildcard imports" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main.go", "foo/foo.go", "bar/bar.go", "bar/rab.go"]
      case ModuleTable.lookup "main.go" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) -> do
          SpecHelpers.lookupDeclaration "foo" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          SpecHelpers.lookupMembers "foo" Import scopeAndFrame heap scopeGraph `shouldBe` Just ["New"]
          SpecHelpers.lookupDeclaration "main" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          SpecHelpers.lookupDeclaration "Bar" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          SpecHelpers.lookupDeclaration "Rab" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
        other -> expectationFailure (show other)

    it "imports with aliases (and side effects only)" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main1.go", "foo/foo.go", "bar/bar.go", "bar/rab.go"]
      case ModuleTable.lookup "main1.go" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) -> do
          SpecHelpers.lookupDeclaration "f" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          SpecHelpers.lookupDeclaration "main" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          -- (lookupDeclaration "f" heap >>= deNamespace heap) `shouldBe` Just ("f",  ["New"])
        other -> expectationFailure (show other)

  where
    fixtures = "test/fixtures/go/analysis/"
    evaluate = evaluateProject @'Language.Go @(Go.Term Loc) ?session Proxy . map (fixtures <>)
