{-# LANGUAGE OverloadedLists #-}
module Analysis.TypeScript.Spec (spec) where

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evaluates TypeScript" $ do
    it "imports with aliased symbols" $ do
      env <- environment . snd <$> evaluate "main.ts"
      env `shouldBe` [ ("bar", addr 0)
                     , ("quz", addr 3)]

    it "imports with qualified names" $ do
      res <- snd <$> evaluate "main1.ts"
      environment res `shouldBe` [ ("b", addr 0)
                                 , ("z", addr 4)
                                 ]

      heapLookup (Address (Precise 0)) (heap res) `shouldBe` ns "b" [ ("baz", addr 1)
                                                                    , ("foo", addr 3) ]
      heapLookup (Address (Precise 4)) (heap res) `shouldBe` ns "z" [ ("baz", addr 1)
                                                                    , ("foo", addr 3) ]

    it "side effect only imports" $ do
      env <- environment . snd <$> evaluate "main2.ts"
      env `shouldBe` mempty

    it "fails exporting symbols not defined in the module" $ do
      v <- fst <$> evaluate "bad-export.ts"
      v `shouldBe` Left "module \"foo.ts\" does not export \"pip\""

  where
    fixtures = "test/fixtures/typescript/analysis/"
    evaluate entry = evalTypeScriptProject (fixtures <> entry)
