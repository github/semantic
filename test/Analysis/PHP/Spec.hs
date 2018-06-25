module Analysis.PHP.Spec (spec) where

import Control.Abstract
import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable (EvalError(..))
import qualified Data.Language as Language
import qualified Data.Abstract.ModuleTable as ModuleTable
import qualified Language.PHP.Assignment as PHP
import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "PHP" $ do
    pure ()
    it "evaluates include and require" $ do
      ((res, heap), _) <- evaluate ["main.php", "foo.php", "bar.php"]
      case ModuleTable.lookup "main.php" <$> res of
        Right (Just (Module _ (addr, env) :| [])) -> do
          heapLookupAll addr heap `shouldBe` Just [unit]
          Env.names env `shouldBe` [ "bar", "foo" ]
        other -> expectationFailure (show other)

    it "evaluates include_once and require_once" $ do
      ((res, heap), _) <- evaluate ["main_once.php", "foo.php", "bar.php"]
      case ModuleTable.lookup "main_once.php" <$> res of
        Right (Just (Module _ (addr, env) :| [])) -> do
          heapLookupAll addr heap `shouldBe` Just [unit]
          Env.names env `shouldBe` [ "bar", "foo" ]
        other -> expectationFailure (show other)

    it "evaluates namespaces" $ do
      ((res, heap), _) <- evaluate ["namespaces.php"]
      case ModuleTable.lookup "namespaces.php" <$> res of
        Right (Just (Module _ (addr, env) :| [])) -> do
          Env.names env `shouldBe` [ "Foo", "NS1" ]

          (derefQName heap ("NS1" :| [])               env >>= deNamespace) `shouldBe` Just ("NS1",  ["Sub1", "b", "c"])
          (derefQName heap ("NS1" :| ["Sub1"])         env >>= deNamespace) `shouldBe` Just ("Sub1", ["Sub2"])
          (derefQName heap ("NS1" :| ["Sub1", "Sub2"]) env >>= deNamespace) `shouldBe` Just ("Sub2", ["f"])
        other -> expectationFailure (show other)

  where
    fixtures = "test/fixtures/php/analysis/"
    evaluate = evalPHPProject . map (fixtures <>)
    evalPHPProject = testEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.PHP) phpParser Language.PHP
