module Analysis.Go.Spec (spec) where

import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable (EvalError(..))
import qualified Data.Language as Language
import qualified Language.Go.Assignment as Go
import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evaluates Go" $ do
    it "imports and wildcard imports" $ do
      (_, (state, Right [(env, _)])) <- evaluate "main.go"
      Env.names env `shouldBe` [ "Bar", "Rab", "foo", "main" ]

      (derefQName (heap state) ("foo" :| []) env >>= deNamespace) `shouldBe` Just ("foo",  ["New"])

    it "imports with aliases (and side effects only)" $ do
      (_, (state, Right [(env, _)])) <- evaluate "main1.go"
      Env.names env `shouldBe` [ "f", "main" ]

      (derefQName (heap state) ("f" :| []) env >>= deNamespace) `shouldBe` Just ("f",  ["New"])

  where
    fixtures = "test/fixtures/go/analysis/"
    evaluate entry = evalGoProject (fixtures <> entry)
    evalGoProject path = testEvaluating <$> evaluateProject (Proxy :: Proxy 'Language.Go) goParser Language.Go path
