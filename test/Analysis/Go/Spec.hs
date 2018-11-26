module Analysis.Go.Spec (spec) where

import Data.Abstract.Evaluatable (EvalError(..))
import qualified Data.Abstract.ModuleTable as ModuleTable
import qualified Data.Language as Language
import qualified Language.Go.Assignment as Go
import SpecHelpers


spec :: TaskConfig -> Spec
spec config = parallel $ do
  describe "Go" $ do
    it "imports and wildcard imports" $ do
      (_, res) <- evaluate ["main.go", "foo/foo.go", "bar/bar.go", "bar/rab.go"]
      case ModuleTable.lookup "main.go" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, valueRef)) :| [])) -> do
          () <$ SpecHelpers.lookupDeclaration "foo" heap scopeGraph `shouldBe` Just ()
          (SpecHelpers.lookupDeclaration "foo" heap scopeGraph >>= objectMembers heap scopeGraph . head) `shouldBe` Just ["New"]
          () <$ SpecHelpers.lookupDeclaration "main" heap scopeGraph `shouldBe` Just ()
          () <$ SpecHelpers.lookupDeclaration "Bar" heap scopeGraph `shouldBe` Just ()
        other -> expectationFailure (show other)

    it "imports with aliases (and side effects only)" $ do
      (_, res) <- evaluate ["main1.go", "foo/foo.go", "bar/bar.go", "bar/rab.go"]
      case ModuleTable.lookup "main1.go" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, valueRef)) :| [])) -> do
          const () <$> SpecHelpers.lookupDeclaration "f" heap scopeGraph `shouldBe` Just ()
          const () <$> SpecHelpers.lookupDeclaration "main" heap scopeGraph `shouldBe` Just ()
          -- (lookupDeclaration "f" heap >>= deNamespace heap) `shouldBe` Just ("f",  ["New"])
        other -> expectationFailure (show other)

  where
    fixtures = "test/fixtures/go/analysis/"
    evaluate = evalGoProject . map (fixtures <>)
    evalGoProject = testEvaluating <=< evaluateProject' config (Proxy :: Proxy 'Language.Go) goParser
