module Analysis.Go.Spec (spec) where

import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable (EvalError(..))
import qualified Data.Language as Language
import qualified Language.Go.Assignment as Go
import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "Go" $ do
    it "imports and wildcard imports" $ do
      ((Right [(_, env)], heap), _) <- evaluate ["main.go"]
      Env.names env `shouldBe` [ "Bar", "Rab", "foo", "main" ]

      (derefQName heap ("foo" :| []) env >>= deNamespace) `shouldBe` Just ("foo",  ["New"])

    it "imports with aliases (and side effects only)" $ do
      ((Right [(_, env)], heap), _) <- evaluate ["main1.go"]
      Env.names env `shouldBe` [ "f", "main" ]

      (derefQName heap ("f" :| []) env >>= deNamespace) `shouldBe` Just ("f",  ["New"])

  where
    fixtures = "test/fixtures/go/analysis/"
    evaluate = evalGoProject . map (fixtures <>)
    evalGoProject = testEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Go) goParser Language.Go
