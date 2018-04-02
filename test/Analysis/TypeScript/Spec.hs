{-# LANGUAGE OverloadedLists #-}
module Analysis.TypeScript.Spec (spec) where

import Data.Abstract.Value
import Data.Map

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evalutes TypeScript" $ do
    it "imports with aliased symbols" $ do
      env <- environment . snd <$> evaluate "main.ts"
      env `shouldBe` [ ("bar", addr 0) ]

    it "imports with qualified names" $ do
      env <- environment . snd <$> evaluate "main1.ts"
      env `shouldBe` [ ("b.baz", addr 0) -- TODO
                     , ("b.foo", addr 2)
                     , ("z.baz", addr 0)
                     , ("z.foo", addr 2)
                     ]

    it "side effect only imports" $ do
      env <- environment . snd <$> evaluate "main2.ts"
      env `shouldBe` mempty

    it "fails exporting symbols not defined in the module" $ do
      v <- fst <$> evaluate "bad-export.ts"
      v `shouldBe` Left "module \"foo\" does not export \"pip\""

  where
    addr = Address . Precise
    fixtures = "test/fixtures/typescript/analysis/"
    evaluate entry = evaluateFiles typescriptParser (takeDirectory entry)
      [ fixtures <> entry
      , fixtures <> "a.ts"
      , fixtures <> "foo.ts"
      , fixtures <> "pip.ts"
      ]
