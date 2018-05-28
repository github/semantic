module Analysis.PHP.Spec (spec) where

import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable (EvalError(..))
import Data.Abstract.Value
import qualified Data.Language as Language
import qualified Language.PHP.Assignment as PHP
import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "PHP" $ do
    it "evaluates include and require" $ do
      env <- environment . snd . fst <$> evaluate "main.php"
      Env.names env `shouldBe` [ "bar", "foo" ]

    it "evaluates include_once and require_once" $ do
      env <- environment . snd . fst <$> evaluate "main_once.php"
      Env.names env `shouldBe` [ "bar", "foo" ]

    it "evaluates namespaces" $ do
      ((_, state), _) <- evaluate "namespaces.php"
      Env.names (environment state) `shouldBe` [ "Foo", "NS1" ]

      (derefQName (heap state) ("NS1" :| [])               (environment state) >>= deNamespace) `shouldBe` Just ("NS1",  ["Sub1", "b", "c"])
      (derefQName (heap state) ("NS1" :| ["Sub1"])         (environment state) >>= deNamespace) `shouldBe` Just ("Sub1", ["Sub2"])
      (derefQName (heap state) ("NS1" :| ["Sub1", "Sub2"]) (environment state) >>= deNamespace) `shouldBe` Just ("Sub2", ["f"])

  where
    fixtures = "test/fixtures/php/analysis/"
    evaluate entry = evalPHPProject (fixtures <> entry)
    evalPHPProject path = testEvaluating . runTermEvaluator @_ @_ @(Value Precise PHP.Term) <$> evaluateProject phpParser Language.PHP Nothing path
