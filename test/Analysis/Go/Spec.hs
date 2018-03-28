{-# LANGUAGE OverloadedLists #-}
module Analysis.Go.Spec (spec) where

import Data.Abstract.Value
import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evalutes Go" $ do
    it "imports and wildcard imports" $ do
      env <- environment . snd <$> evaluate "main.go"
      env `shouldBe` [ (qualifiedName ["foo", "New"], addr 0)
                     , (qualifiedName ["Rab"], addr 1)
                     , (qualifiedName ["Bar"], addr 2)
                     , (qualifiedName ["main"], addr 3)
                     ]

    it "imports with aliases (and side effects only)" $ do
      env <- environment . snd <$> evaluate "main1.go"
      env `shouldBe` [ (qualifiedName ["f", "New"], addr 0)
                     , (qualifiedName ["main"], addr 3) -- addr 3 is due to side effects of
                                                        -- eval'ing `import _ "./bar"` which
                                                        -- used addr 1 & 2.
                     ]

  where
    addr = Address . Precise
    fixtures = "test/fixtures/go/analysis/"
    evaluate entry = evaluateFiles goParser
      [ fixtures <> entry
      , fixtures <> "foo/foo.go"
      , fixtures <> "bar/bar.go"
      , fixtures <> "bar/rab.go"
      ]
