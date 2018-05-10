{-# LANGUAGE OverloadedLists #-}
module Analysis.PHP.Spec (spec) where

import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable (EvalError(..))
import qualified Language.PHP.Assignment as PHP
import qualified Data.Language as Language

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "PHP" $ do
    it "evaluates include and require" $ do
      env <- environment . snd <$> evaluate "main.php"
      Env.names env `shouldBe` [ "bar", "foo" ]

    it "evaluates include_once and require_once" $ do
      env <- environment . snd <$> evaluate "main_once.php"
      Env.names env `shouldBe` [ "bar", "foo" ]

    it "evaluates namespaces" $ do
      res <- snd <$> evaluate "namespaces.php"
      Env.names (environment res) `shouldBe` [ "Foo", "NS1" ]

      (derefQName (heap res) ("NS1" :| [])               (environment res) >>= deNamespace) `shouldBe` Just ("NS1",  ["Sub1", "b", "c"])
      (derefQName (heap res) ("NS1" :| ["Sub1"])         (environment res) >>= deNamespace) `shouldBe` Just ("Sub1", ["Sub2"])
      (derefQName (heap res) ("NS1" :| ["Sub1", "Sub2"]) (environment res) >>= deNamespace) `shouldBe` Just ("Sub2", ["f"])

  where
    fixtures = "test/fixtures/php/analysis/"
    evaluate entry = evalPHPProject (fixtures <> entry)
    evalPHPProject path = testEvaluating <$> evaluateProject phpParser Language.PHP Nothing path
