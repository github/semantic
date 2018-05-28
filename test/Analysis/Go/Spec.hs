module Analysis.Go.Spec (spec) where

import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable (EvalError(..))
import qualified Language.Go.Assignment as Go
import qualified Data.Language as Language

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evaluates Go" $ do
    it "imports and wildcard imports" $ do
      ((_, state), _) <- evaluate "main.go"
      Env.names (environment state) `shouldBe` [ "Bar", "Rab", "foo", "main" ]

      (derefQName (heap state) ("foo" :| []) (environment state) >>= deNamespace) `shouldBe` Just ("foo",  ["New"])

    it "imports with aliases (and side effects only)" $ do
      ((_, state), _) <- evaluate "main1.go"
      Env.names (environment state) `shouldBe` [ "f", "main" ]

      (derefQName (heap state) ("f" :| []) (environment state) >>= deNamespace) `shouldBe` Just ("f",  ["New"])

  where
    fixtures = "test/fixtures/go/analysis/"
    evaluate entry = evalGoProject (fixtures <> entry)
    evalGoProject path = testEvaluating <$> evaluateProject goParser Language.Go Nothing path
