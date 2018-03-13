{-# LANGUAGE TypeApplications #-}
module Analysis.Go.Spec (spec) where

import Data.Abstract.Value
import Data.Map

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evalutes Go" $ do
    it "imports and wildcard imports" $ do
      res <- evaluate "main.go"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["foo", "New"], addr 0)
            , (qualifiedName ["Rab"], addr 1)
            , (qualifiedName ["Bar"], addr 2)
            , (qualifiedName ["main"], addr 3)
            ]
      assertEnvironment res expectedEnv

    it "imports with aliases (and side effects only)" $ do
      res <- evaluate "main1.go"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["f", "New"], addr 0)
            , (qualifiedName ["main"], addr 3) -- addr 3 is due to side effects of
                                               -- eval'ing `import _ "./bar"` which
                                               -- used addr 1 & 2.
            ]
      assertEnvironment res expectedEnv

  where
    assertEnvironment result expectedEnv = case result of
      Left e -> expectationFailure ("Evaluating expected to succeed, but failed with: " <> e)
      Right res -> let Just (Interface _ env) = prjValue @(Interface Precise) res in env `shouldBe` expectedEnv

    addr = Address . Precise
    fixtures = "test/fixtures/go/analysis/"
    evaluate entry = fst . fst . fst . fst <$>
      evaluateFiles @GoValue goParser
        [ fixtures <> entry
        , fixtures <> "foo/foo.go"
        , fixtures <> "bar/bar.go"
        , fixtures <> "bar/rab.go"
        ]
