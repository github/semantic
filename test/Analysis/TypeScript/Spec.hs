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

spec :: TaskConfig -> Spec
spec config = parallel $ do
  describe "TypeScript" $ do
    it "imports with aliased symbols" $ do
      (_, (_, res)) <- evaluate ["main.ts", "foo.ts", "a.ts", "foo/b.ts"]
      case ModuleTable.lookup "main.ts" <$> res of
        Right (Just (Module _ (env, _) :| [])) -> Env.names env `shouldBe` [ "bar", "quz" ]
        other -> expectationFailure (show other)

    it "imports with qualified names" $ do
      (_, (heap, res)) <- evaluate ["main1.ts", "foo.ts", "a.ts"]
      case ModuleTable.lookup "main1.ts" <$> res of
        Right (Just (Module _ (env, _) :| [])) -> do
          Env.names env `shouldBe` [ "b", "z" ]

          (derefQName heap ("b" :| []) env >>= deNamespace heap) `shouldBe` Just ("b", [ "baz", "foo" ])
          (derefQName heap ("z" :| []) env >>= deNamespace heap) `shouldBe` Just ("z", [ "baz", "foo" ])
        other -> expectationFailure (show other)

    it "side effect only imports" $ do
      (_, (_, res)) <- evaluate ["main2.ts", "a.ts", "foo.ts"]
      case ModuleTable.lookup "main2.ts" <$> res of
        Right (Just (Module _ (env, _) :| [])) -> env `shouldBe` lowerBound
        other -> expectationFailure (show other)

    it "fails exporting symbols not defined in the module" $ do
      (_, (_, res)) <- evaluate ["bad-export.ts", "pip.ts", "a.ts", "foo.ts"]
      res `shouldBe` Left (SomeExc (inject @EvalError (ExportError "foo.ts" (name "pip"))))

    it "evaluates early return statements" $ do
      (_, (heap, res)) <- evaluate ["early-return.ts"]
      case ModuleTable.lookup "early-return.ts" <$> res of
        Right (Just (Module _ (_, addr) :| [])) -> heapLookupAll addr heap `shouldBe` Just [Value.Float (Number.Decimal 123.0)]
        other -> expectationFailure (show other)

  where
    fixtures = "test/fixtures/typescript/analysis/"
    evaluate = evalTypeScriptProject . map (fixtures <>)
    evalTypeScriptProject = testEvaluating <=< evaluateProject' config (Proxy :: Proxy 'Language.TypeScript) typescriptParser
