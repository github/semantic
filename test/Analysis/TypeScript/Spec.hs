{-# LANGUAGE OverloadedLists, TypeApplications #-}
module Analysis.TypeScript.Spec (spec) where

import Data.Abstract.Value
import Data.Map

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evalutes TypeScript" $ do
    it "imports with aliased symbols" $ do
      env <- findEnv <$> evaluate "main.ts"
      env `shouldBe` [ (qualifiedName ["bar"], addr 0) ]

    it "imports with qualified names" $ do
      env <- findEnv <$> evaluate "main1.ts"
      env `shouldBe` [ (qualifiedName ["b", "baz"], addr 0)
                     , (qualifiedName ["b", "foo"], addr 2)
                     , (qualifiedName ["z", "baz"], addr 0)
                     , (qualifiedName ["z", "foo"], addr 2)
                     ]

    it "side effect only imports" $ do
      env <- findEnv <$> evaluate "main2.ts"
      env `shouldBe` mempty

    it "fails exporting symbols not defined in the module" $ do
      v <- findValue <$> evaluate "bad-export.ts"
      v `shouldBe` Left "module \"foo\" does not export \"pip\""

  where
    addr = Address . Precise
    fixtures = "test/fixtures/typescript/analysis/"
    evaluate entry = evaluateFiles typescriptParser
      [ fixtures <> entry
      , fixtures <> "a.ts"
      , fixtures <> "foo.ts"
      , fixtures <> "pip.ts"
      ]
