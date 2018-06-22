module Analysis.PHP.Spec (spec) where

import Control.Abstract
import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable (EvalError(..))
import qualified Data.Language as Language
import qualified Language.PHP.Assignment as PHP
import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "PHP" $ do
    it "evaluates include and require" $ do
      (_, (_, Right [(env, res)])) <- evaluate "main.php"
      res `shouldBe` unit
      Env.names env `shouldBe` [ "bar", "foo" ]

    it "evaluates include_once and require_once" $ do
      (_, (_, Right [(env, res)])) <- evaluate "main_once.php"
      res `shouldBe` unit
      Env.names env `shouldBe` [ "bar", "foo" ]

    it "evaluates namespaces" $ do
      (_, (state, Right [(env, _)])) <- evaluate "namespaces.php"
      Env.names env `shouldBe` [ "Foo", "NS1" ]

      (derefQName (heap state) ("NS1" :| [])               env >>= deNamespace) `shouldBe` Just ("NS1",  ["Sub1", "b", "c"])
      (derefQName (heap state) ("NS1" :| ["Sub1"])         env >>= deNamespace) `shouldBe` Just ("Sub1", ["Sub2"])
      (derefQName (heap state) ("NS1" :| ["Sub1", "Sub2"]) env >>= deNamespace) `shouldBe` Just ("Sub2", ["f"])

  where
    fixtures = "test/fixtures/php/analysis/"
    evaluate entry = evalPHPProject (fixtures <> entry)
    evalPHPProject path = testEvaluating <$> evaluateProject (Proxy :: Proxy 'Language.PHP) phpParser Language.PHP path
