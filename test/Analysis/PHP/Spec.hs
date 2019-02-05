module Analysis.PHP.Spec (spec) where

import           Control.Abstract
import           Data.Abstract.Evaluatable (EvalError (..))
import qualified Data.Abstract.ModuleTable as ModuleTable
import qualified Data.Abstract.Value.Concrete as Value
import qualified Data.Language as Language
import qualified Language.PHP.Assignment as PHP
import           SpecHelpers


spec :: TaskSession -> Spec
spec session = parallel $ do
  describe "PHP" $ do
    xit "evaluates include and require" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main.php", "foo.php", "bar.php"]
      case ModuleTable.lookup "main.php" <$> res of
        Right (Just (Module _ (scopeAndFrame, value))) -> do
          value `shouldBe` Value.Unit
          const () <$> SpecHelpers.lookupDeclaration "bar" scopeAndFrame heap scopeGraph `shouldBe` Just ()
          const () <$> SpecHelpers.lookupDeclaration "foo" scopeAndFrame heap scopeGraph `shouldBe` Just ()
        other -> expectationFailure (show other)

    xit "evaluates include_once and require_once" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main_once.php", "foo.php", "bar.php"]
      case ModuleTable.lookup "main_once.php" <$> res of
        Right (Just (Module _ (scopeAndFrame, value))) -> do
          value `shouldBe` Value.Unit
          const () <$> SpecHelpers.lookupDeclaration "bar" scopeAndFrame heap scopeGraph `shouldBe` Just ()
          const () <$> SpecHelpers.lookupDeclaration "foo" scopeAndFrame heap scopeGraph `shouldBe` Just ()
        other -> expectationFailure (show other)

    xit "evaluates namespaces" $ do
      (scopeGraph, (heap, res)) <- evaluate ["namespaces.php"]
      case ModuleTable.lookup "namespaces.php" <$> res of
        Right (Just (Module _ (scopeAndFrame, value))) -> do
          const () <$> SpecHelpers.lookupDeclaration "Foo" scopeAndFrame heap scopeGraph `shouldBe` Just ()
          const () <$> SpecHelpers.lookupDeclaration "NS1" scopeAndFrame heap scopeGraph `shouldBe` Just ()

          undefined
          -- (derefQName heap ("NS1" :| [])               env >>= deNamespace heap) `shouldBe` Just ("NS1",  ["Sub1", "b", "c"])
          -- (derefQName heap ("NS1" :| ["Sub1"])         env >>= deNamespace heap) `shouldBe` Just ("Sub1", ["Sub2"])
          -- (derefQName heap ("NS1" :| ["Sub1", "Sub2"]) env >>= deNamespace heap) `shouldBe` Just ("Sub2", ["f"])
        other -> expectationFailure (show other)

  where
    fixtures = "test/fixtures/php/analysis/"
    evaluate = evalPHPProject . map (fixtures <>)
    evalPHPProject = testEvaluating <=< evaluateProject' session (Proxy :: Proxy 'Language.PHP) phpParser
