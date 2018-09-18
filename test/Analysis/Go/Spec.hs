module Analysis.Go.Spec (spec) where

import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable (EvalError(..))
import qualified Data.Abstract.ModuleTable as ModuleTable
import qualified Data.Language as Language
import qualified Language.Go.Assignment as Go
import SpecHelpers


spec :: TaskConfig -> Spec
spec config = parallel $ do
  describe "Go" $ do
    it "imports and wildcard imports" $ do
      (_, (heap, res)) <- evaluate ["main.go", "foo/foo.go", "bar/bar.go", "bar/rab.go"]
      case ModuleTable.lookup "main.go" <$> res of
        Right (Just (Module _ (_, (env, addr)) :| [])) -> do
          Env.names env `shouldBe` [ "Bar", "Rab", "foo", "main" ]
          (derefQName heap ("foo" :| []) env >>= deNamespace heap) `shouldBe` Just ("foo",  ["New"])
        other -> expectationFailure (show other)

    it "imports with aliases (and side effects only)" $ do
      (_, (heap, res)) <- evaluate ["main1.go", "foo/foo.go", "bar/bar.go", "bar/rab.go"]
      case ModuleTable.lookup "main1.go" <$> res of
        Right (Just (Module _ (_, (env, addr)) :| [])) -> do
          Env.names env `shouldBe` [ "f", "main" ]
          (derefQName heap ("f" :| []) env >>= deNamespace heap) `shouldBe` Just ("f",  ["New"])
        other -> expectationFailure (show other)

  where
    fixtures = "test/fixtures/go/analysis/"
    evaluate = evalGoProject . map (fixtures <>)
    evalGoProject = testEvaluating <=< evaluateProject' config (Proxy :: Proxy 'Language.Go) goParser
