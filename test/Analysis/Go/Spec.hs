{-# LANGUAGE OverloadedLists #-}
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
      res <- snd <$> evaluate "main.go"
      Env.names (environment res) `shouldBe` [ "Bar", "Rab", "foo", "main" ]

      (derefQName (heap res) ("foo" :| []) (environment res) >>= deNamespace) `shouldBe` Just ("foo",  ["New"])

    it "imports with aliases (and side effects only)" $ do
      res <- snd <$> evaluate "main1.go"
      Env.names (environment res) `shouldBe` [ "f", "main" ]

      (derefQName (heap res) ("f" :| []) (environment res) >>= deNamespace) `shouldBe` Just ("f",  ["New"])

  where
    fixtures = "test/fixtures/go/analysis/"
    evaluate entry = evalGoProject (fixtures <> entry)
    evalGoProject path = testEvaluating <$> evaluateProject goParser Language.Go Nothing path
