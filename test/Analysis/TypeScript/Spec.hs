module Analysis.TypeScript.Spec (spec) where

import Control.Arrow ((&&&))
import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable
import Data.Abstract.Number as Number
import Data.Abstract.Package (PackageInfo(..))
import Data.Abstract.Module (ModuleInfo(..))
import qualified Data.Abstract.ModuleTable as ModuleTable
import Data.Abstract.Value.Concrete as Value
import qualified Data.Language as Language
import qualified Data.List.NonEmpty as NonEmpty
import Data.Sum
import SpecHelpers
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import qualified Data.Abstract.Heap       as Heap
import Data.Text (pack)
import qualified Language.TypeScript.Assignment as TypeScript
import Data.Quieterm
import Data.Location

spec :: TaskConfig -> Spec
spec config = parallel $ do
  describe "TypeScript" $ do
    it "imports with aliased symbols" $ do
      (_, res) <- evaluate ["main.ts", "foo.ts", "a.ts", "foo/b.ts"]
      case ModuleTable.lookup "main.ts" <$> res of
        Right (Just (Module _ (scopeGraph, _) :| [])) -> do
          (fmap (const ()) <$> ScopeGraph.lookupScopePath "bar" scopeGraph) `shouldBe` Just (ScopeGraph.EPath ScopeGraph.Import () (ScopeGraph.EPath ScopeGraph.Import () (ScopeGraph.DPath (ScopeGraph.Declaration "bar") (Heap.Position 1) )))
          (fmap (const ()) <$> ScopeGraph.lookupScopePath "quz" scopeGraph) `shouldBe` Just (ScopeGraph.EPath ScopeGraph.Import () (ScopeGraph.DPath (ScopeGraph.Declaration "quz") (Heap.Position 1) ))
        other -> expectationFailure (show other)

    it "imports with qualified names" $ do
      (_, res) <- evaluate ["main1.ts", "foo.ts", "a.ts"]
      case ModuleTable.lookup "main1.ts" <$> res of
        Right (Just (Module _ (scopeGraph, _) :| [])) -> do
          -- Env.names env `shouldBe` [ "b", "z" ]
          (fmap (const ()) <$> ScopeGraph.lookupScopePath "b" scopeGraph) `shouldBe` Just (ScopeGraph.DPath (ScopeGraph.Declaration "b") (Heap.Position 1))
          (fmap (const ()) <$> ScopeGraph.lookupScopePath "quz" scopeGraph) `shouldBe` Just (ScopeGraph.DPath (ScopeGraph.Declaration "z") (Heap.Position 2))

          -- (Heap.lookupDeclaration "b" heap  >>= deNamespace heap) `shouldBe` Just ("b", [ "baz", "foo" ])
          -- (Heap.lookupDeclaration "z" heap >>= deNamespace heap) `shouldBe` Just ("z", [ "baz", "foo" ])
          (fmap (const ()) <$> ScopeGraph.lookupScopePath "baz" scopeGraph) `shouldBe` Just (ScopeGraph.EPath ScopeGraph.Import () (ScopeGraph.EPath ScopeGraph.Import () (ScopeGraph.DPath (ScopeGraph.Declaration "baz") (Heap.Position 1) )))
          (fmap (const ()) <$> ScopeGraph.lookupScopePath "foo" scopeGraph) `shouldBe` Just (ScopeGraph.EPath ScopeGraph.Import () (ScopeGraph.DPath (ScopeGraph.Declaration "foo") (Heap.Position 1) ))
        other -> expectationFailure (show other)

    it "stores function declaration in scope graph" $ do
      (_, res) <- evaluate ["a.ts"]
      case ModuleTable.lookup "a.ts" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, valueRef)) :| [])) -> do
          fmap (const ()) <$> ScopeGraph.lookupScopePath "baz" scopeGraph `shouldBe` Just (ScopeGraph.DPath (ScopeGraph.Declaration "baz") (Heap.Position 0))
          valueRef `shouldBe` Rval Unit
        other -> expectationFailure (show other)

    it "imports functions" $ do
      (_, res) <- evaluate ["main4.ts", "foo.ts"]
      case ModuleTable.lookup "main4.ts" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, valueRef)) :| [])) -> do
          fmap (const ()) <$> ScopeGraph.lookupScopePath "foo" scopeGraph `shouldBe` Just (ScopeGraph.EPath ScopeGraph.Import ()  . ScopeGraph.EPath ScopeGraph.Import () $ ScopeGraph.DPath (ScopeGraph.Declaration "foo") (Heap.Position 0))
          valueRef `shouldBe` Rval (String $ pack "\"this is the foo function\"")
        other -> expectationFailure (show other)

    it "side effect only imports dont expose exports" $ do
      (_, res) <- evaluate ["main3.ts", "a.ts"]
      case ModuleTable.lookup "main3.ts" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, valueRef)) :| [])) -> do
          fmap (const ()) <$> ScopeGraph.lookupScopePath "baz" scopeGraph `shouldBe` Nothing
          valueRef `shouldBe` Rval Unit
          const () <$> Heap.currentFrame heap `shouldBe` Just ()
          Heap.heapSize heap `shouldBe` 1
        other -> expectationFailure (show other)

    it "side effect only imports" $ do
      (_, res) <- evaluate ["main2.ts", "a.ts", "foo.ts"]
      case ModuleTable.lookup "main2.ts" <$> res of
        Right (Just (Module _ (_, (heap, _)) :| [])) -> heap `shouldSatisfy` Heap.isHeapEmpty
        other -> expectationFailure (show other)

    it "fails exporting symbols not defined in the module" $ do
      (_, res) <- evaluate ["bad-export.ts", "pip.ts", "a.ts", "foo.ts"]
      res `shouldBe` Left (SomeError (inject @(BaseError (EvalError Precise (Value (Quieterm (Sum TypeScript.Syntax) Location) Precise))) (BaseError (ModuleInfo "foo.ts") emptySpan (ExportError "foo.ts" (name "pip")))))

    it "evaluates early return statements" $ do
      (_, res) <- evaluate ["early-return.ts"]
      case ModuleTable.lookup "early-return.ts" <$> res of
        Right (Just (Module _ (scopeGraph, _) :| [])) -> (fmap (const ()) <$> ScopeGraph.lookupScopePath "foo" scopeGraph) `shouldBe` Just (ScopeGraph.DPath (ScopeGraph.Declaration "foo") (Heap.Position 1))
        -- SpecHelpers.lookupDeclaration undefined heap scopeGraph `shouldBe` Just [Value.Float (Number.Decimal 123.0)]
        other -> expectationFailure (show other)

    it "evaluates sequence expressions" $ do
      (_, res) <- evaluate ["sequence-expression.ts"]
      case ModuleTable.lookup "sequence-expression.ts" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, _)) :| [])) -> do
          -- (Heap.lookupDeclaration "x" heap) `shouldBe` Just (Value.Float (Number.Decimal 3.0))
          let value = SpecHelpers.lookupDeclaration "x" heap scopeGraph
          value `shouldBe` Just [ Value.Float (Number.Decimal 3.0) ]

          -- Env.names env `shouldBe` [ "x" ]
          (fmap (const ()) <$> ScopeGraph.lookupScopePath "x" scopeGraph) `shouldBe` Just (ScopeGraph.DPath (ScopeGraph.Declaration "x") (Heap.Position 1))
        other -> expectationFailure (show other)

    it "evaluates void expressions" $ do
      (_, res) <- evaluate ["void.ts"]
      case ModuleTable.lookup "void.ts" <$> res of
        Right (Just (Module _ (_, (_, value)) :| [])) -> value `shouldBe` Rval Null
        other -> expectationFailure (show other)

    it "evaluates delete" $ do
      (_, res) <- evaluate ["delete.ts"]
      case ModuleTable.lookup "delete.ts" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, value)) :| [])) -> do
          value `shouldBe` Rval Unit
          SpecHelpers.lookupDeclaration "x" heap scopeGraph `shouldBe` Just []
        other -> expectationFailure (show other)

    it "evaluates await" $ do
      (_, res) <- evaluate ["await.ts"]
      case ModuleTable.lookup "await.ts" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, value)) :| [])) -> do
          SpecHelpers.lookupDeclaration "f2" heap scopeGraph `shouldBe` Just []
          SpecHelpers.lookupDeclaration "y" heap scopeGraph `shouldBe` Nothing
        other -> expectationFailure (show other)

    it "evaluates BOr statements" $ do
      (_, res) <- evaluate ["bor.ts"]
      case ModuleTable.lookup "bor.ts" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, value)) :| [])) ->
          value `shouldBe` Rval (Value.Integer (Number.Integer 3))
        other -> expectationFailure (show other)

    it "evaluates BAnd statements" $ do
      (_, res) <- evaluate ["band.ts"]
      case ModuleTable.lookup "band.ts" <$> res of
        Right (Just (Module _ (_, (_, value)) :| [])) -> value `shouldBe` Rval (Value.Integer (Number.Integer 0))
        other -> expectationFailure (show other)

    it "evaluates BXOr statements" $ do
      (_, res) <- evaluate ["bxor.ts"]
      case ModuleTable.lookup "bxor.ts" <$> res of
        Right (Just (Module _ (_, (_, value)) :| [])) -> value `shouldBe` Rval (Value.Integer (Number.Integer 3))
        other -> expectationFailure (show other)

    it "evaluates LShift statements" $ do
      (_, res) <- evaluate ["lshift.ts"]
      case ModuleTable.lookup "lshift.ts" <$> res of
        Right (Just (Module _ (_, (_, value)) :| [])) -> value `shouldBe` Rval (Value.Integer (Number.Integer 4))
        other -> expectationFailure (show other)

    it "evaluates RShift statements" $ do
      (_, res) <- evaluate ["rshift.ts"]
      case ModuleTable.lookup "rshift.ts" <$> res of
        Right (Just (Module _ (_, (_, value)) :| [])) -> value `shouldBe` Rval (Value.Integer (Number.Integer 0))
        other -> expectationFailure (show other)

    it "evaluates Complement statements" $ do
      (_, res) <- evaluate ["complement.ts"]
      case ModuleTable.lookup "complement.ts" <$> res of
        Right (Just (Module _ (_, (_, value)) :| [])) -> value `shouldBe` Rval (Value.Integer (Number.Integer (-2)))
        other -> expectationFailure (show other)


  where
    fixtures = "test/fixtures/typescript/analysis/"
    evaluate = evalTypeScriptProject . map (fixtures <>)
    evalTypeScriptProject = testEvaluating <=< (evaluateProject' config (Proxy :: Proxy 'Language.TypeScript) typescriptParser)
