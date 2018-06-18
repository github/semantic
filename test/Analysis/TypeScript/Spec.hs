module Analysis.TypeScript.Spec (spec) where

import Control.Arrow ((&&&))
import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable
import qualified Language.TypeScript.Assignment as TypeScript
import Data.Abstract.Value as Value
import Data.Abstract.Number as Number
import qualified Data.Language as Language
import Data.Sum

import SpecHelpers

spec :: Spec
spec = parallel $ do
  describe "evaluates TypeScript" $ do
    it "imports with aliased symbols" $ do
      ((Right [(_, env)], _), _) <- evaluate "main.ts"
      Env.names env `shouldBe` [ "bar", "quz" ]

    it "imports with qualified names" $ do
      ((Right [(_, env)], state), _) <- evaluate "main1.ts"
      Env.names env `shouldBe` [ "b", "z" ]

      (derefQName (heap state) ("b" :| []) env >>= deNamespace) `shouldBe` Just ("b", [ "baz", "foo" ])
      (derefQName (heap state) ("z" :| []) env >>= deNamespace) `shouldBe` Just ("z", [ "baz", "foo" ])

    it "side effect only imports" $ do
      ((res, _), _) <- evaluate "main2.ts"
      fmap snd <$> res `shouldBe` Right [lowerBound]

    it "fails exporting symbols not defined in the module" $ do
      ((res, _), _) <- evaluate "bad-export.ts"
      res `shouldBe` Left (SomeExc (inject @EvalError (ExportError "foo.ts" (name "pip"))))

    it "evaluates early return statements" $ do
      ((res, _), _) <- evaluate "early-return.ts"
      fmap fst <$> res `shouldBe` Right [Value.Float (Number.Decimal 123.0)]

  where
    fixtures = "test/fixtures/typescript/analysis/"
    evaluate entry = evalTypeScriptProject (fixtures <> entry)
    evalTypeScriptProject path = testEvaluating <$> evaluateProject (Proxy :: Proxy 'Language.TypeScript) typescriptParser Language.TypeScript path
