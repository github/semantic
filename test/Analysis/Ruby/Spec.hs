{-# LANGUAGE DataKinds, ImplicitParams, OverloadedStrings, TypeApplications #-}
{-# OPTIONS_GHC -O0 #-}
module Analysis.Ruby.Spec (spec) where

import           Control.Abstract (Declaration (..), ScopeError (..))
import           Control.Effect.Resumable (SomeError (..))
import           Data.Abstract.Evaluatable
import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Number as Number
import           Data.Abstract.Value.Concrete as Value
import qualified Data.Language as Language
import           Data.Sum
import qualified Language.Ruby.Term as Ruby
import           Source.Loc
import           SpecHelpers


spec :: (?session :: TaskSession) =>  Spec
spec = do
  describe "Ruby" $ do
    it "evaluates require_relative" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main.rb", "foo.rb"]
      case ModuleTable.lookup "main.rb" <$> res of
        Right (Just (Module _ (scopeAndFrame, value))) -> do
          value `shouldBe` Value.Integer (Number.Integer 1)
          SpecHelpers.lookupDeclaration "foo" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
        other -> expectationFailure (show other)

    it "evaluates load" $ do
      (scopeGraph, (heap, res)) <- evaluate ["load.rb", "foo.rb"]
      case ModuleTable.lookup "load.rb" <$> res of
        Right (Just (Module _ (scopeAndFrame, value))) -> do
          value `shouldBe` Value.Integer (Number.Integer 1)
          SpecHelpers.lookupDeclaration "foo" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
        other -> expectationFailure (show other)

    it "evaluates load with wrapper" $ do
      (_, (_, res)) <- evaluate ["load-wrap.rb", "foo.rb"]
      res `shouldBe` Left (SomeError (inject @(BaseError (ScopeError Precise)) (BaseError (ModuleInfo "load-wrap.rb" Language.Ruby mempty) (Span (Pos 3 1) (Pos 3 7)) (LookupPathError (Declaration "foo")))))

    it "evaluates subclass" $ do
      (scopeGraph, (heap, res)) <- evaluate ["subclass.rb"]
      case ModuleTable.lookup "subclass.rb" <$> res of
        Right (Just (Module _ (scopeAndFrame, value))) -> do
          value `shouldBe` String "\"<bar>\""
          SpecHelpers.lookupDeclaration "Bar" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          SpecHelpers.lookupDeclaration "Foo" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          SpecHelpers.lookupMembers "Bar" Superclass scopeAndFrame heap scopeGraph `shouldBe` Just ["baz", "foo", "inspect"]
        other -> expectationFailure (show other)

    it "evaluates modules" $ do
      (scopeGraph, (heap, res)) <- evaluate ["modules.rb"]
      case ModuleTable.lookup "modules.rb" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) -> do
          SpecHelpers.lookupDeclaration "Bar" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
        other -> expectationFailure (show other)

    it "handles break correctly" $ do
      (_, (_, res)) <- evaluate ["break.rb"]
      case ModuleTable.lookup "break.rb" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Value.Integer (Number.Integer 3)
        other                                       -> expectationFailure (show other)

    it "handles next correctly" $ do
      (_, (_, res)) <- evaluate ["next.rb"]
      case ModuleTable.lookup "next.rb" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Value.Integer (Number.Integer 8)
        other                                       -> expectationFailure (show other)

    it "calls functions with arguments" $ do
      (_, (_, res)) <- evaluate ["call.rb"]
      case ModuleTable.lookup "call.rb" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Value.Integer (Number.Integer 579)
        other                                       -> expectationFailure (show other)

    it "evaluates early return statements" $ do
      (_, (_, res)) <- evaluate ["early-return.rb"]
      case ModuleTable.lookup "early-return.rb" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Value.Integer (Number.Integer 123)
        other                                       -> expectationFailure (show other)

    it "has prelude" $ do
      (_, (_, res)) <- evaluate ["preluded.rb"]
      case ModuleTable.lookup "preluded.rb" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` String "\"<foo>\""
        other                                       -> expectationFailure (show other)

    it "evaluates __LINE__" $ do
      (_, (_, res)) <- evaluate ["line.rb"]
      case ModuleTable.lookup "line.rb" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Value.Integer (Number.Integer 4)
        other                                       -> expectationFailure (show other)

    it "resolves builtins used in the prelude" $ do
      (scopeGraph, (heap, res)) <- evaluate ["puts.rb"]
      case ModuleTable.lookup "puts.rb" <$> res of
        Right (Just (Module _ (scopeAndFrame, value))) -> do
          value `shouldBe` Unit
          SpecHelpers.lookupDeclaration "puts" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
        other -> expectationFailure (show other)

  where
    fixtures = "test/fixtures/ruby/analysis/"
    evaluate = evaluateProject @'Language.Ruby @(Ruby.Term Loc) ?session Proxy . map (fixtures <>)
