{-# LANGUAGE DataKinds, ImplicitParams, OverloadedStrings, TypeApplications #-}
{-# OPTIONS_GHC -O0 #-}

module Analysis.TypeScript.Spec (spec) where

import           Data.Syntax.Statement (StatementBlock(..))
import qualified Data.Abstract.ScopeGraph as ScopeGraph (AccessControl(..))
import           Control.Abstract.ScopeGraph hiding (AccessControl(..))
import           Data.Abstract.Evaluatable
import qualified Data.Abstract.Heap as Heap
import           Data.Abstract.Module (ModuleInfo (..))
import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Number as Number
import           Data.Abstract.Package (PackageInfo (..))
import           Data.Abstract.Value.Concrete as Concrete
import qualified Data.Language as Language
import           Data.Scientific (scientific)
import           Data.Sum
import           Data.Text (pack)
import qualified Language.TypeScript.Term as TypeScript
import           Source.Loc
import           SpecHelpers

spec :: (?session :: TaskSession) => Spec
spec = do
  describe "TypeScript" $ do
    it "qualified export from" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main6.ts", "baz.ts", "foo.ts"]
      case ModuleTable.lookup "main6.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) -> do
          SpecHelpers.lookupDeclaration "foo" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
        other -> expectationFailure (show other)

    it "imports with aliased symbols" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main.ts", "foo.ts", "foo/b.ts"]
      case ModuleTable.lookup "main.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) -> do
          SpecHelpers.lookupDeclaration "bar" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          SpecHelpers.lookupDeclaration "quz" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust

        other -> expectationFailure (show other)

    it "imports with qualified names" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main1.ts", "foo.ts", "a.ts"]
      case ModuleTable.lookup "main1.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) -> do
          SpecHelpers.lookupDeclaration "b" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          SpecHelpers.lookupDeclaration "z" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust

          lookupMembers "b" Import scopeAndFrame heap scopeGraph `shouldBe` Just  [ "baz", "foo" ]
          lookupMembers "z" Import scopeAndFrame heap scopeGraph `shouldBe` Just  [ "baz", "foo" ]

          SpecHelpers.lookupDeclaration "baz" scopeAndFrame heap scopeGraph `shouldBe` Nothing
          SpecHelpers.lookupDeclaration "foo" scopeAndFrame heap scopeGraph `shouldBe` Nothing
        other -> expectationFailure (show other)

    it "stores function declaration in scope graph" $ do
      (scopeGraph, (heap, res)) <- evaluate ["a.ts"]
      case ModuleTable.lookup "a.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame, value))) -> do
          SpecHelpers.lookupDeclaration "baz" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          value `shouldBe` Unit
        other -> expectationFailure (show other)

    it "imports functions" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main4.ts", "foo.ts"]
      case ModuleTable.lookup "main4.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame, value))) -> do
          SpecHelpers.lookupDeclaration "foo" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          value `shouldBe` String (pack "\"this is the foo function\"")
        other -> expectationFailure (show other)

    it "side effect only imports dont expose exports" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main3.ts", "a.ts"]
      case ModuleTable.lookup "main3.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame, value))) -> do
          SpecHelpers.lookupDeclaration "baz" scopeAndFrame heap scopeGraph `shouldBe` Nothing
          value `shouldBe` Unit
          Heap.heapSize heap `shouldBe` 4
        other -> expectationFailure (show other)

    it "fails exporting symbols not defined in the module" $ do
      (_, (_, res)) <- evaluate ["bad-export.ts", "pip.ts", "a.ts", "foo.ts"]
      res `shouldBe` Left (SomeError (inject @(BaseError (ScopeError Precise)) (BaseError (ModuleInfo "bad-export.ts" Language.TypeScript mempty) (Span (Pos 2 1) (Pos 2 28)) ImportReferenceError)))

    it "evaluates early return statements" $ do
      (scopeGraph, (heap, res)) <- evaluate ["early-return.ts"]
      case ModuleTable.lookup "early-return.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) ->
          SpecHelpers.lookupDeclaration "foo" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
        other -> expectationFailure (show other)

    it "evaluates sequence expressions" $ do
      (scopeGraph, (heap, res)) <- evaluate ["sequence-expression.ts"]
      case ModuleTable.lookup "sequence-expression.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) ->
          SpecHelpers.lookupDeclaration "x" scopeAndFrame heap scopeGraph `shouldBe` Just [ Concrete.Float (Number.Decimal (scientific 3 0)) ]
        other -> expectationFailure (show other)

    it "evaluates void expressions" $ do
      (_, (_, res)) <- evaluate ["void.ts"]
      case ModuleTable.lookup "void.ts" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Null
        other                              -> expectationFailure (show other)

    it "evaluates delete" $ do
      (scopeGraph, (heap, res)) <- evaluate ["delete.ts"]
      case ModuleTable.lookup "delete.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame, value))) -> do
          value `shouldBe` Unit
          SpecHelpers.lookupDeclaration "x" scopeAndFrame heap scopeGraph `shouldBe` Nothing
        other -> expectationFailure (show other)

    it "evaluates await" $ do
      (scopeGraph, (heap, res)) <- evaluate ["await.ts"]
      case ModuleTable.lookup "await.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) -> do
          -- Test that f2 is in the scopegraph and heap.
          SpecHelpers.lookupDeclaration "f2" scopeAndFrame heap scopeGraph `shouldSatisfy` isJust
          -- Test we can't reference y from outside the function
          SpecHelpers.lookupDeclaration "y" scopeAndFrame heap scopeGraph `shouldBe` Nothing
        other -> expectationFailure (show other)

    it "evaluates BOr statements" $ do
      (_, (_, res)) <- evaluate ["bor.ts"]
      case ModuleTable.lookup "bor.ts" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Concrete.Integer (Number.Integer 3)
        other                              -> expectationFailure (show other)

    it "evaluates BAnd statements" $ do
      (_, (_, res)) <- evaluate ["band.ts"]
      case ModuleTable.lookup "band.ts" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Concrete.Integer (Number.Integer 0)
        other                              -> expectationFailure (show other)

    it "evaluates BXOr statements" $ do
      (_, (_, res)) <- evaluate ["bxor.ts"]
      case ModuleTable.lookup "bxor.ts" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Concrete.Integer (Number.Integer 3)
        other                              -> expectationFailure (show other)

    it "evaluates LShift statements" $ do
      (_, (_, res)) <- evaluate ["lshift.ts"]
      case ModuleTable.lookup "lshift.ts" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Concrete.Integer (Number.Integer 4)
        other                              -> expectationFailure (show other)

    it "evaluates RShift statements" $ do
      (_, (_, res)) <- evaluate ["rshift.ts"]
      case ModuleTable.lookup "rshift.ts" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Concrete.Integer (Number.Integer 0)
        other                              -> expectationFailure (show other)

    it "evaluates Complement statements" $ do
      (_, (_, res)) <- evaluate ["complement.ts"]
      case ModuleTable.lookup "complement.ts" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Concrete.Integer (Number.Integer (-2))
        other                              -> expectationFailure (show other)

    it "uniquely tracks public fields for instances" $ do
      (_, (_, res)) <- evaluate ["class1.ts", "class2.ts"]
      case ModuleTable.lookup "class1.ts" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` (Concrete.Float (Number.Decimal 9.0))
        other                              -> expectationFailure (show other)

    it "member access of private field definition throws AccessControlError" $ do
      (_, (_, res)) <- evaluate ["access_control/adder.ts", "access_control/private_field_definition.ts"]
      let expected = Left (SomeError (inject @TypeScriptEvalError (BaseError (ModuleInfo "private_field_definition.ts" Language.TypeScript mempty) (Span (Pos 4 1) (Pos 4 6)) (AccessControlError ("foo", ScopeGraph.Public) ("y", ScopeGraph.Private) (Concrete.Float (Decimal 2.0))))))
      res `shouldBe` expected

    it "member access of private static field definition throws AccessControlError" $ do
      (_, (_, res)) <- evaluate ["access_control/adder.ts", "access_control/private_static_field_definition.ts"]
      let expected = Left (SomeError (inject @TypeScriptEvalError (BaseError (ModuleInfo "private_static_field_definition.ts" Language.TypeScript mempty) (Span (Pos 3 1) (Pos 3 8)) (AccessControlError ("Adder", ScopeGraph.Public) ("z", ScopeGraph.Private) Unit))))
      res `shouldBe` expected

    it "member access of private methods throws AccessControlError" $ do
      (_, (_, res)) <- evaluate ["access_control/adder.ts", "access_control/private_method.ts"]
      let expected = Left (SomeError (inject @TypeScriptEvalError (BaseError (ModuleInfo "private_method.ts" Language.TypeScript mempty) (Span (Pos 4 1) (Pos 4 16)) (AccessControlError ("foo", ScopeGraph.Public) ("private_add", ScopeGraph.Private) (Closure (PackageInfo "access_control" mempty) (ModuleInfo "adder.ts" Language.TypeScript mempty) (Just "private_add") Nothing [] (Right (TypeScript.Term (In (Loc (Range 146 148) (Span (Pos 7 27) (Pos 7 29))) (inject (StatementBlock []))))) (Precise 20) (Precise 18))))))
      res `shouldBe` expected

  where
    fixtures = "test/fixtures/typescript/analysis/"
    evaluate = evaluateProject @'Language.TypeScript @(TypeScript.Term Loc) ?session Proxy . map (fixtures <>)

type TypeScriptEvalError = BaseError (EvalError (TypeScript.Term Loc) Precise (Concrete.Value (TypeScript.Term Loc) Precise))
