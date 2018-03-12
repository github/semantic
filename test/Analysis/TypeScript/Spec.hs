{-# LANGUAGE TypeApplications #-}
module Analysis.TypeScript.Spec where

import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Value as Value
import Data.AST
import Data.Map as Map
import Data.Record
import Data.Semigroup
import Data.Term
import Data.Union
import Parsing.Parser

import SpecHelpers
import Test.Hspec (Spec, describe, it, xit, parallel, pendingWith)
import Test.Hspec.Expectations.Pretty
import qualified Language.TypeScript.Assignment as TypeScript


type TypeScriptValue = Value Precise (Term (Union TypeScript.Syntax) (Record Location))

spec :: Spec
spec = parallel $ do
  describe "evalutes TypeScript" $ do
    it "imports with aliased symbols" $ do
      res <- evaluate "main.ts"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["bar"], addr 0)
            ]
      assertEnvironment res expectedEnv

    it "imports with qualified names" $ do
      res <- evaluate "main1.ts"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["b", "baz"], addr 0)
            , (qualifiedName ["b", "foo"], addr 2)
            , (qualifiedName ["z", "baz"], addr 0)
            , (qualifiedName ["z", "foo"], addr 2)
            ]
      assertEnvironment res expectedEnv

    it "side effect only imports" $ do
      res <- evaluate "main2.ts"
      let expectedEnv = Environment $ fromList []
      assertEnvironment res expectedEnv

  where
    assertEnvironment result expectedEnv = case result of
      Left e -> expectationFailure ("Evaluating expected to succeed, but failed with: " <> e)
      Right res -> let Just (Interface _ env) = prj @(Interface Precise) res in env `shouldBe` expectedEnv

    addr = Address . Precise
    fixtures = "test/fixtures/typescript/analysis/"
    evaluate entry = fst . fst . fst . fst <$>
      evaluateFiles @TypeScriptValue typescriptParser
        [ fixtures <> entry
        , fixtures <> "a.ts"
        , fixtures <> "foo.ts"
        ]
