module Analysis.TypeScript.Spec (spec) where

import           Control.Abstract.ScopeGraph
import           Control.Abstract.Value as Value
import           Control.Arrow ((&&&))
import           Data.Abstract.Evaluatable
import qualified Data.Abstract.Heap as Heap
import           Data.Abstract.Module (ModuleInfo (..))
import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Number as Number
import           Data.Abstract.Package (PackageInfo (..))
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import           Data.Abstract.Value.Concrete as Value
import qualified Data.Language as Language
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Location
import           Data.Quieterm
import           Data.Scientific (scientific)
import           Data.Sum
import           Data.Text (pack)
import qualified Language.TypeScript.Assignment as TypeScript
import           SpecHelpers

spec :: TaskConfig -> Spec
spec config = parallel $ do
  describe "TypeScript" $ do
    it "qualified export from" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main6.ts", "baz.ts", "foo.ts"]
      case ModuleTable.lookup "main6.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) -> do
          () <$ SpecHelpers.lookupDeclaration "foo" scopeAndFrame heap scopeGraph `shouldBe` Just ()
        other -> expectationFailure (show other)

    it "imports with aliased symbols" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main.ts", "foo.ts", "foo/b.ts"]
      case ModuleTable.lookup "main.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) -> do
          const () <$> SpecHelpers.lookupDeclaration "bar" scopeAndFrame heap scopeGraph `shouldBe` Just ()
          const () <$> SpecHelpers.lookupDeclaration "quz" scopeAndFrame heap scopeGraph `shouldBe` Just ()

        other -> expectationFailure (show other)

    it "imports with qualified names" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main1.ts", "foo.ts", "a.ts"]
      case ModuleTable.lookup "main1.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) -> do
          () <$ SpecHelpers.lookupDeclaration "b" scopeAndFrame heap scopeGraph `shouldBe` Just ()
          () <$ SpecHelpers.lookupDeclaration "z" scopeAndFrame heap scopeGraph `shouldBe` Just ()

          lookupMembers "b" Import scopeAndFrame heap scopeGraph `shouldBe` Just  [ "baz", "foo" ]
          lookupMembers "z" Import scopeAndFrame heap scopeGraph `shouldBe` Just  [ "baz", "foo" ]

          () <$ SpecHelpers.lookupDeclaration "baz" scopeAndFrame heap scopeGraph `shouldBe` Nothing
          () <$ SpecHelpers.lookupDeclaration "foo" scopeAndFrame heap scopeGraph `shouldBe` Nothing
        other -> expectationFailure (show other)

    it "stores function declaration in scope graph" $ do
      (scopeGraph, (heap, res)) <- evaluate ["a.ts"]
      case ModuleTable.lookup "a.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame, value))) -> do
          const () <$> SpecHelpers.lookupDeclaration "baz" scopeAndFrame heap scopeGraph `shouldBe` Just ()
          value `shouldBe` Unit
        other -> expectationFailure (show other)

    it "imports functions" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main4.ts", "foo.ts"]
      case ModuleTable.lookup "main4.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame, value))) -> do
          const () <$> SpecHelpers.lookupDeclaration "foo" scopeAndFrame heap scopeGraph `shouldBe` Just ()
          value `shouldBe` String (pack "\"this is the foo function\"")
        other -> expectationFailure (show other)

    it "side effect only imports dont expose exports" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main3.ts", "a.ts"]
      case ModuleTable.lookup "main3.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame@(currentScope, currentFrame), value))) -> do
          () <$ SpecHelpers.lookupDeclaration "baz" scopeAndFrame heap scopeGraph `shouldBe` Nothing
          value `shouldBe` Unit
          Heap.heapSize heap `shouldBe` 4
        other -> expectationFailure (show other)

    it "fails exporting symbols not defined in the module" $ do
      (_, (_, res)) <- evaluate ["bad-export.ts", "pip.ts", "a.ts", "foo.ts"]
      res `shouldBe` Left (SomeError (inject @(BaseError (ScopeError Precise)) (BaseError (ModuleInfo "bad-export.ts") (Span (Pos 2 1) (Pos 2 28)) ImportReferenceError)))

    it "evaluates early return statements" $ do
      (scopeGraph, (heap, res)) <- evaluate ["early-return.ts"]
      case ModuleTable.lookup "early-return.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame, value))) ->
          const () <$> SpecHelpers.lookupDeclaration "foo" scopeAndFrame heap scopeGraph `shouldBe` Just ()
        other -> expectationFailure (show other)

    it "evaluates sequence expressions" $ do
      (scopeGraph, (heap, res)) <- evaluate ["sequence-expression.ts"]
      case ModuleTable.lookup "sequence-expression.ts" <$> res of
        Right (Just (Module _ (scopeAndFrame, value))) ->
          SpecHelpers.lookupDeclaration "x" scopeAndFrame heap scopeGraph `shouldBe` Just [ Value.float (scientific 3 0) ]
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
        Right (Just (Module _ (scopeAndFrame, value))) -> do
          -- Test that f2 is in the scopegraph and heap.
          const () <$> SpecHelpers.lookupDeclaration "f2" scopeAndFrame heap scopeGraph `shouldBe` Just ()
          -- Test we can't reference y from outside the function
          SpecHelpers.lookupDeclaration "y" scopeAndFrame heap scopeGraph `shouldBe` Nothing
        other -> expectationFailure (show other)

    it "evaluates BOr statements" $ do
      (_, (_, res)) <- evaluate ["bor.ts"]
      case ModuleTable.lookup "bor.ts" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Value.Integer (Number.Integer 3)
        other                              -> expectationFailure (show other)

    it "evaluates BAnd statements" $ do
      (_, (_, res)) <- evaluate ["band.ts"]
      case ModuleTable.lookup "band.ts" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Value.Integer (Number.Integer 0)
        other                              -> expectationFailure (show other)

    it "evaluates BXOr statements" $ do
      (_, (_, res)) <- evaluate ["bxor.ts"]
      case ModuleTable.lookup "bxor.ts" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Value.Integer (Number.Integer 3)
        other                              -> expectationFailure (show other)

    it "evaluates LShift statements" $ do
      (_, (_, res)) <- evaluate ["lshift.ts"]
      case ModuleTable.lookup "lshift.ts" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Value.Integer (Number.Integer 4)
        other                              -> expectationFailure (show other)

    it "evaluates RShift statements" $ do
      (_, (_, res)) <- evaluate ["rshift.ts"]
      case ModuleTable.lookup "rshift.ts" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Value.Integer (Number.Integer 0)
        other                              -> expectationFailure (show other)

    it "evaluates Complement statements" $ do
      (_, (_, res)) <- evaluate ["complement.ts"]
      case ModuleTable.lookup "complement.ts" <$> res of
        Right (Just (Module _ (_, value))) -> value `shouldBe` Value.Integer (Number.Integer (-2))
        other                              -> expectationFailure (show other)


  where
    fixtures = "test/fixtures/typescript/analysis/"
    evaluate = evalTypeScriptProject . map (fixtures <>)
    evalTypeScriptProject = testEvaluating <=< (evaluateProject' config (Proxy :: Proxy 'Language.TypeScript) typescriptParser)
