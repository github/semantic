module Analysis.Ruby.Spec (spec) where

import qualified Data.Abstract.ModuleTable as ModuleTable
import Data.Abstract.Number as Number
import Data.Abstract.Value.Concrete as Value
import Data.AST
import Control.Effect.Resumable (SomeError(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Sum
import qualified Language.Ruby.Assignment as Ruby
import qualified Data.Language as Language
import Data.Abstract.Evaluatable
import Control.Abstract (ScopeError(..), Declaration(..))

import SpecHelpers


spec :: TaskConfig -> Spec
spec config = parallel $ do
  describe "Ruby" $ do
    it "evaluates require_relative" $ do
      (_, res) <- evaluate ["main.rb", "foo.rb"]
      case ModuleTable.lookup "main.rb" <$> res of
        Right (Just (Module _ (scopeAndFrame, valueRef) :| [])) -> do
          valueRef `shouldBe` Rval (Value.Integer (Number.Integer 1))
          const () <$> lookupDeclaration "foo" scopeAndFrame heap scopeGraph `shouldBe` Just ()
        other -> expectationFailure (show other)

    it "evaluates load" $ do
      (_, res) <- evaluate ["load.rb", "foo.rb"]
      case ModuleTable.lookup "load.rb" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, ((currentScope, currentFrame), valueRef))) :| [])) -> do
          valueRef `shouldBe` Rval (Value.Integer (Number.Integer 1))
          const () <$> SpecHelpers.lookupDeclaration "foo" heap scopeGraph `shouldBe` Just ()
        other -> expectationFailure (show other)

    it "evaluates load with wrapper" $ do
      (_, res) <- evaluate ["load-wrap.rb", "foo.rb"]
      res `shouldBe` Left (SomeError (inject @(BaseError (ScopeError Precise)) (BaseError (ModuleInfo "load-wrap.rb") emptySpan (ScopeError (Declaration "foo") emptySpan))))

    it "evaluates subclass" $ do
      (_, res) <- evaluate ["subclass.rb"]
      case ModuleTable.lookup "subclass.rb" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, valueRef)) :| [])) -> do
          valueRef `shouldBe` Rval (String "\"<bar>\"")
          const () <$> SpecHelpers.lookupDeclaration "Bar" heap scopeGraph `shouldBe` Just ()
          const () <$> SpecHelpers.lookupDeclaration "Foo" heap scopeGraph `shouldBe` Just ()

          -- (lookupDeclaration "Bar" heap >>= deNamespace heap) `shouldBe` Just ("Bar",  ["baz", "inspect", "foo"])
        other -> expectationFailure (show other)

    it "evaluates modules" $ do
      (_, res) <- evaluate ["modules.rb"]
      case ModuleTable.lookup "modules.rb" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, valueRef)) :| [])) -> do
          valueRef `shouldBe` Rval (String "\"<hello>\"")
          const () <$> SpecHelpers.lookupDeclaration "Bar" heap scopeGraph `shouldBe` Just ()
        other -> expectationFailure (show other)

    it "handles break correctly" $ do
      (_, res) <- evaluate ["break.rb"]
      case ModuleTable.lookup "break.rb" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, valueRef)) :| [])) -> valueRef `shouldBe` Rval (Value.Integer (Number.Integer 3))
        other -> expectationFailure (show other)

    it "handles next correctly" $ do
      (_, res) <- evaluate ["next.rb"]
      case ModuleTable.lookup "next.rb" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, valueRef)) :| [])) -> valueRef `shouldBe` Rval (Value.Integer (Number.Integer 8))
        other -> expectationFailure (show other)

    it "calls functions with arguments" $ do
      (_, res) <- evaluate ["call.rb"]
      case ModuleTable.lookup "call.rb" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, valueRef)) :| [])) -> valueRef `shouldBe` Rval (Value.Integer (Number.Integer 579))
        other -> expectationFailure (show other)

    it "evaluates early return statements" $ do
      (_, res) <- evaluate ["early-return.rb"]
      case ModuleTable.lookup "early-return.rb" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, valueRef)) :| [])) -> valueRef `shouldBe` Rval (Value.Integer (Number.Integer 123))
        other -> expectationFailure (show other)

    it "has prelude" $ do
      (_, res) <- evaluate ["preluded.rb"]
      case ModuleTable.lookup "preluded.rb" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, valueRef)) :| [])) -> valueRef `shouldBe` Rval (String "\"<foo>\"")
        other -> expectationFailure (show other)

    it "evaluates __LINE__" $ do
      (_, res) <- evaluate ["line.rb"]
      case ModuleTable.lookup "line.rb" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, valueRef)) :| [])) -> valueRef `shouldBe` Rval (Value.Integer (Number.Integer 4))
        other -> expectationFailure (show other)

    it "resolves builtins used in the prelude" $ do
      (traces, res) <- evaluate ["puts.rb"]
      case ModuleTable.lookup "puts.rb" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, valueRef)) :| [])) -> do
          valueRef `shouldBe` Rval Unit
          traces `shouldContain` ["String \"\\\"hello\\\"\""]
        other -> expectationFailure (show other)

  where
    fixtures = "test/fixtures/ruby/analysis/"
    evaluate = evalRubyProject . map (fixtures <>)
    evalRubyProject = testEvaluating <=< evaluateProject' config (Proxy :: Proxy 'Language.Ruby) rubyParser
