module Analysis.TypeScript.Spec (spec) where

import Control.Arrow ((&&&))
import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable
import Data.Abstract.Number as Number
import qualified Data.Abstract.ModuleTable as ModuleTable
import Data.Abstract.Value.Concrete as Value
import qualified Data.Language as Language
import qualified Data.List.NonEmpty as NonEmpty
import Data.Sum
import SpecHelpers
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import qualified Data.Abstract.Heap       as Heap

spec :: TaskConfig -> Spec
spec config = parallel $ do
  describe "TypeScript" $ do
    it "imports with aliased symbols" $ do
      (_, res) <- evaluate ["main.ts", "foo.ts", "a.ts", "foo/b.ts"]
      case ModuleTable.lookup "main.ts" <$> res of
        Right (Just (Module _ (scopeGraph, _) :| [])) -> do
          (fmap (const ()) <$> ScopeGraph.lookupScopePath "bar" scopeGraph) `shouldBe` Just (ScopeGraph.EPath ScopeGraph.Import () (ScopeGraph.EPath ScopeGraph.Import () (ScopeGraph.DPath (ScopeGraph.Declaration "bar") (Position 1) )))
          (fmap (const ()) <$> ScopeGraph.lookupScopePath "quz" scopeGraph) `shouldBe` Just (ScopeGraph.EPath ScopeGraph.Import () (ScopeGraph.DPath (ScopeGraph.Declaration "quz") (Position 1) ))
        other -> expectationFailure (show other)

    it "imports with qualified names" $ do
      (_, res) <- evaluate ["main1.ts", "foo.ts", "a.ts"]
      case ModuleTable.lookup "main1.ts" <$> res of
        Right (Just (Module _ (scopeGraph, _) :| [])) -> do
          -- Env.names env `shouldBe` [ "b", "z" ]
          (fmap (const ()) <$> ScopeGraph.lookupScopePath "b" scopeGraph) `shouldBe` Just (ScopeGraph.DPath (ScopeGraph.Declaration "b") (Position 1))
          (fmap (const ()) <$> ScopeGraph.lookupScopePath "quz" scopeGraph) `shouldBe` Just (ScopeGraph.DPath (ScopeGraph.Declaration "z") (Position 2))

          -- (lookupDeclaration "b" heap  >>= deNamespace heap) `shouldBe` Just ("b", [ "baz", "foo" ])
          -- (lookupDeclaration "z" heap >>= deNamespace heap) `shouldBe` Just ("z", [ "baz", "foo" ])
          (fmap (const ()) <$> ScopeGraph.lookupScopePath "baz" scopeGraph) `shouldBe` Just (ScopeGraph.EPath ScopeGraph.Import () (ScopeGraph.EPath ScopeGraph.Import () (ScopeGraph.DPath (ScopeGraph.Declaration "baz") (Position 1) )))
          (fmap (const ()) <$> ScopeGraph.lookupScopePath "foo" scopeGraph) `shouldBe` Just (ScopeGraph.EPath ScopeGraph.Import () (ScopeGraph.DPath (ScopeGraph.Declaration "foo") (Position 1) ))
        other -> expectationFailure (show other)

    it "side effect only imports" $ do
      (_, res) <- evaluate ["main2.ts", "a.ts", "foo.ts"]
      case ModuleTable.lookup "main2.ts" <$> res of
        Right (Just (Module _ (_, (env, _)) :| [])) -> env `shouldBe` lowerBound
        other -> expectationFailure (show other)

    it "fails exporting symbols not defined in the module" $ do
      (_, res) <- evaluate ["bad-export.ts", "pip.ts", "a.ts", "foo.ts"]
      res `shouldBe` Left (SomeError (inject @(BaseError EvalError) (BaseError (ModuleInfo "foo.ts") emptySpan (ExportError "foo.ts" (name "pip")))))

    it "evaluates early return statements" $ do
      (_, res) <- evaluate ["early-return.ts"]
      case ModuleTable.lookup "early-return.ts" <$> res of
        Right (Just (Module _ (scopeGraph, _) :| [])) -> (fmap (const ()) <$> ScopeGraph.lookupScopePath "foo" scopeGraph) `shouldBe` Just (ScopeGraph.DPath (ScopeGraph.Declaration "foo") (Position 1))
        -- heapLookupAll addr heap `shouldBe` Just [Value.Float (Number.Decimal 123.0)]
        other -> expectationFailure (show other)

    it "evaluates sequence expressions" $ do
      (_, res) <- evaluate ["sequence-expression.ts"]
      case ModuleTable.lookup "sequence-expression.ts" <$> res of
        Right (Just (Module _ (scopeGraph, (heap, _)) :| [])) -> do
          -- (lookupDeclaration "x" heap) `shouldBe` Just (Value.Float (Number.Decimal 3.0))
          frameAddress <- Heap.lookupDeclaration "x" scopeGraph heap
          getSlot frameAddress heap `shouldBe` Just (Value.Float (Number.Decimal 3.0))

          -- Env.names env `shouldBe` [ "x" ]
          (fmap (const ()) <$> ScopeGraph.lookupScopePath "x" scopeGraph) `shouldBe` Just (ScopeGraph.DPath (ScopeGraph.Declaration "x") (Position 1))
        other -> expectationFailure (show other)

    it "evaluates void expressions" $ do
      (_, res) <- evaluate ["void.ts"]
      case ModuleTable.lookup "void.ts" <$> res of
        Right (Just (Module _ (_, (_, addr)) :| [])) -> heapLookupAll addr heap `shouldBe` Just [Null]
        other -> expectationFailure (show other)

    it "evaluates delete" $ do
      (_, res) <- evaluate ["delete.ts"]
      case ModuleTable.lookup "delete.ts" <$> res of
        Right (Just (Module _ (_, (env, addr)) :| [])) -> do
          heapLookupAll addr heap `shouldBe` Just [Unit]
          (lookupDeclaration "x" heap) `shouldBe` Nothing
          Env.names env `shouldBe` [ "x" ]
        other -> expectationFailure (show other)

    it "evaluates await" $ do
      (_, res) <- evaluate ["await.ts"]
      case ModuleTable.lookup "await.ts" <$> res of
        Right (Just (Module _ (_, (env, addr)) :| [])) -> do
          Env.names env `shouldBe` [ "f2" ]
          (lookupDeclaration "y" heap) `shouldBe` Nothing
        other -> expectationFailure (show other)

    it "evaluates BOr statements" $ do
      (_, res) <- evaluate ["bor.ts"]
      case ModuleTable.lookup "bor.ts" <$> res of
        Right (Just (Module _ (_, (_, addr)) :| [])) -> heapLookupAll addr heap `shouldBe` Just [Value.Integer (Number.Integer 3)]
        other -> expectationFailure (show other)

    it "evaluates BAnd statements" $ do
      (_, res) <- evaluate ["band.ts"]
      case ModuleTable.lookup "band.ts" <$> res of
        Right (Just (Module _ (_, (_, addr)) :| [])) -> heapLookupAll addr heap `shouldBe` Just [Value.Integer (Number.Integer 0)]
        other -> expectationFailure (show other)

    it "evaluates BXOr statements" $ do
      (_, res) <- evaluate ["bxor.ts"]
      case ModuleTable.lookup "bxor.ts" <$> res of
        Right (Just (Module _ (_, (_, addr)) :| [])) -> heapLookupAll addr heap `shouldBe` Just [Value.Integer (Number.Integer 3)]
        other -> expectationFailure (show other)

    it "evaluates LShift statements" $ do
      (_, res) <- evaluate ["lshift.ts"]
      case ModuleTable.lookup "lshift.ts" <$> res of
        Right (Just (Module _ (_, (_, addr)) :| [])) -> heapLookupAll addr heap `shouldBe` Just [Value.Integer (Number.Integer 4)]
        other -> expectationFailure (show other)

    it "evaluates RShift statements" $ do
      (_, res) <- evaluate ["rshift.ts"]
      case ModuleTable.lookup "rshift.ts" <$> res of
        Right (Just (Module _ (_, (_, addr)) :| [])) -> heapLookupAll addr heap `shouldBe` Just [Value.Integer (Number.Integer 0)]
        other -> expectationFailure (show other)

    it "evaluates Complement statements" $ do
      (_, res) <- evaluate ["complement.ts"]
      case ModuleTable.lookup "complement.ts" <$> res of
        Right (Just (Module _ (_, (_, addr)) :| [])) -> heapLookupAll addr heap `shouldBe` Just [Value.Integer (Number.Integer (-2))]
        other -> expectationFailure (show other)


  where
    fixtures = "test/fixtures/typescript/analysis/"
    evaluate = evalTypeScriptProject . map (fixtures <>)
    evalTypeScriptProject = testEvaluating <=< (evaluateProject' config (Proxy :: Proxy 'Language.TypeScript) typescriptParser)
