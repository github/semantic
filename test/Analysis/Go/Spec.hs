{-# LANGUAGE TypeApplications #-}
module Analysis.Go.Spec (spec) where

import Data.Abstract.Value
import Data.Map

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evalutes Go" $ do
    it "imports and wildcard imports" $ do
      env <- evaluate "main.go"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["foo", "New"], addr 0)
            , (qualifiedName ["Rab"], addr 1)
            , (qualifiedName ["Bar"], addr 2)
            , (qualifiedName ["main"], addr 3)
            ]
      env `shouldBe` expectedEnv

    it "imports with aliases (and side effects only)" $ do
      env <- evaluate "main1.go"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["f", "New"], addr 0)
            , (qualifiedName ["main"], addr 3) -- addr 3 is due to side effects of
                                               -- eval'ing `import _ "./bar"` which
                                               -- used addr 1 & 2.
            ]
      env `shouldBe` expectedEnv

  where
    addr = Address . Precise
    fixtures = "test/fixtures/go/analysis/"
    evaluate entry = snd . fst . fst . fst . fst <$>
      evaluateFiles @(Value Precise) goParser
        [ fixtures <> entry
        , fixtures <> "foo/foo.go"
        , fixtures <> "bar/bar.go"
        , fixtures <> "bar/rab.go"
        ]
