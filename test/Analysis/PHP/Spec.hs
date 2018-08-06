module Analysis.PHP.Spec (spec) where

import Control.Abstract
import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable (EvalError(..))
import qualified Data.Language as Language
import qualified Data.Abstract.ModuleTable as ModuleTable
import qualified Language.PHP.Assignment as PHP
import SpecHelpers


spec :: TaskConfig -> Spec
spec config = parallel $ do
  describe "PHP" $ do
    it "evaluates include and require" $ do
      (_, (heap, res)) <- evaluate ["main.php", "foo.php", "bar.php"]
      case ModuleTable.lookup "main.php" <$> res of
        Right (Just (Module _ (env, addr) :| [])) -> do
          heapLookupAll addr heap `shouldBe` Just [unit]
          Env.names env `shouldBe` [ "bar", "foo" ]
        other -> expectationFailure (show other)

    it "evaluates include_once and require_once" $ do
      (_, (heap, res)) <- evaluate ["main_once.php", "foo.php", "bar.php"]
      case ModuleTable.lookup "main_once.php" <$> res of
        Right (Just (Module _ (env, addr) :| [])) -> do
          heapLookupAll addr heap `shouldBe` Just [unit]
          Env.names env `shouldBe` [ "bar", "foo" ]
        other -> expectationFailure (show other)

    it "evaluates namespaces" $ do
      (_, (heap, res)) <- evaluate ["namespaces.php"]
      case ModuleTable.lookup "namespaces.php" <$> res of
        Right (Just (Module _ (env, addr) :| [])) -> do
          Env.names env `shouldBe` [ "Foo", "NS1" ]

          (derefQName heap ("NS1" :| [])               env >>= deNamespace heap) `shouldBe` Just ("NS1",  ["Sub1", "b", "c"])
          (derefQName heap ("NS1" :| ["Sub1"])         env >>= deNamespace heap) `shouldBe` Just ("Sub1", ["Sub2"])
          (derefQName heap ("NS1" :| ["Sub1", "Sub2"]) env >>= deNamespace heap) `shouldBe` Just ("Sub2", ["f"])
        other -> expectationFailure (show other)

  where
    fixtures = "test/fixtures/php/analysis/"
    evaluate = evalPHPProject . map (fixtures <>)
    evalPHPProject = testEvaluating <=< evaluateProject' config (Proxy :: Proxy 'Language.PHP) phpParser
