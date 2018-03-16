module Analysis.TypeScript.Spec (spec) where

import Data.Abstract.Value
import Data.Map

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evalutes TypeScript" $ do
    it "imports with aliased symbols" $ do
      env <- evaluate "main.ts"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["bar"], addr 0)
            ]
      env `shouldBe` expectedEnv

    it "imports with qualified names" $ do
      env <- evaluate "main1.ts"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["b", "baz"], addr 0)
            , (qualifiedName ["b", "foo"], addr 2)
            , (qualifiedName ["z", "baz"], addr 0)
            , (qualifiedName ["z", "foo"], addr 2)
            ]
      env `shouldBe` expectedEnv

    it "side effect only imports" $ do
      env <- evaluate "main2.ts"
      env `shouldBe` Environment (fromList [])

    it "fails exporting symbols not defined in the module" $ do
      env <- fst <$> evaluate' "bad-export.ts"
      env `shouldBe` Left "module \"foo\" does not export \"pip\""

  where
    addr = Address . Precise
    fixtures = "test/fixtures/typescript/analysis/"
    evaluate entry = snd <$> evaluate' entry
    evaluate' entry = fst . fst . fst . fst <$>
      evaluateFiles typescriptParser
        [ fixtures <> entry
        , fixtures <> "a.ts"
        , fixtures <> "foo.ts"
        , fixtures <> "pip.ts"
        ]
