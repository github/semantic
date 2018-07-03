module Analysis.Ruby.Spec (spec) where

import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable
import qualified Data.Abstract.ModuleTable as ModuleTable
import Data.Abstract.Number as Number
import Data.Abstract.Value.Concrete as Value
import Data.AST
import Control.Monad.Effect (SomeExc(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Sum
import qualified Language.Ruby.Assignment as Ruby
import qualified Data.Language as Language

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "Ruby" $ do
    it "evaluates require_relative" $ do
      ((res, heap), _) <- evaluate ["main.rb", "foo.rb"]
      case ModuleTable.lookup "main.rb" <$> res of
        Right (Just (Module _ (addr, env) :| [])) -> do
          heapLookupAll addr heap `shouldBe` Just [Value.Integer (Number.Integer 1)]
          Env.names env `shouldContain` [ "foo" ]
        other -> expectationFailure (show other)

    it "evaluates load" $ do
      ((res, heap), _) <- evaluate ["load.rb", "foo.rb"]
      case ModuleTable.lookup "load.rb" <$> res of
        Right (Just (Module _ (addr, env) :| [])) -> do
          heapLookupAll addr heap `shouldBe` Just [Value.Integer (Number.Integer 1)]
          Env.names env `shouldContain` [ "foo" ]
        other -> expectationFailure (show other)

    it "evaluates load with wrapper" $ do
      ((res, _), _) <- evaluate ["load-wrap.rb", "foo.rb"]
      res `shouldBe` Left (SomeExc (inject @(EnvironmentError Precise) (FreeVariable "foo")))

    it "evaluates subclass" $ do
      ((res, heap), _) <- evaluate ["subclass.rb"]
      case ModuleTable.lookup "subclass.rb" <$> res of
        Right (Just (Module _ (addr, env) :| [])) -> do
          heapLookupAll addr heap `shouldBe` Just [String "\"<bar>\""]
          Env.names env `shouldContain` [ "Bar", "Foo" ]

          (derefQName heap ("Bar" :| []) env >>= deNamespace) `shouldBe` Just ("Bar",  ["baz", "foo", "inspect"])
        other -> expectationFailure (show other)

    it "evaluates modules" $ do
      ((res, heap), _) <- evaluate ["modules.rb"]
      case ModuleTable.lookup "modules.rb" <$> res of
        Right (Just (Module _ (addr, env) :| [])) -> do
          heapLookupAll addr heap `shouldBe` Just [String "\"<hello>\""]
          Env.names env `shouldContain` [ "Bar" ]
        other -> expectationFailure (show other)

    it "handles break correctly" $ do
      ((res, heap), _) <- evaluate ["break.rb"]
      case ModuleTable.lookup "break.rb" <$> res of
        Right (Just (Module _ (addr, env) :| [])) -> heapLookupAll addr heap `shouldBe` Just [Value.Integer (Number.Integer 3)]
        other -> expectationFailure (show other)

    it "handles next correctly" $ do
      ((res, heap), _) <- evaluate ["next.rb"]
      case ModuleTable.lookup "next.rb" <$> res of
        Right (Just (Module _ (addr, env) :| [])) -> heapLookupAll addr heap `shouldBe` Just [Value.Integer (Number.Integer 8)]
        other -> expectationFailure (show other)

    it "calls functions with arguments" $ do
      ((res, heap), _) <- evaluate ["call.rb"]
      case ModuleTable.lookup "call.rb" <$> res of
        Right (Just (Module _ (addr, env) :| [])) -> heapLookupAll addr heap `shouldBe` Just [Value.Integer (Number.Integer 579)]
        other -> expectationFailure (show other)

    it "evaluates early return statements" $ do
      ((res, heap), _) <- evaluate ["early-return.rb"]
      case ModuleTable.lookup "early-return.rb" <$> res of
        Right (Just (Module _ (addr, env) :| [])) -> heapLookupAll addr heap `shouldBe` Just [Value.Integer (Number.Integer 123)]
        other -> expectationFailure (show other)

    it "has prelude" $ do
      ((res, heap), _) <- evaluate ["preluded.rb"]
      case ModuleTable.lookup "preluded.rb" <$> res of
        Right (Just (Module _ (addr, env) :| [])) -> heapLookupAll addr heap `shouldBe` Just [String "\"<foo>\""]
        other -> expectationFailure (show other)

    it "evaluates __LINE__" $ do
      ((res, heap), _) <- evaluate ["line.rb"]
      case ModuleTable.lookup "line.rb" <$> res of
        Right (Just (Module _ (addr, env) :| [])) -> heapLookupAll addr heap `shouldBe` Just [Value.Integer (Number.Integer 4)]
        other -> expectationFailure (show other)

    it "resolves builtins used in the prelude" $ do
      ((res, heap), traces) <- evaluate ["puts.rb"]
      case ModuleTable.lookup "puts.rb" <$> res of
        Right (Just (Module _ (addr, env) :| [])) -> do
          heapLookupAll addr heap `shouldBe` Just [Unit]
          traces `shouldContain` [ "\"hello\"" ]
        other -> expectationFailure (show other)

  where
    ns n = Just . Latest . Last . Just . Namespace n
    fixtures = "test/fixtures/ruby/analysis/"
    evaluate = evalRubyProject . map (fixtures <>)
    evalRubyProject = testEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Ruby) rubyParser Language.Ruby
