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
            , (qualifiedName ["Bar"], addr 1)
            , (qualifiedName ["main"], addr 2)
            ]
      assertEnvironment res expectedEnv

    it "imports with aliases (and side effects only)" $ do
      res <- evaluate "main1.go"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["f", "New"], addr 0)
            , (qualifiedName ["main"], addr 2) -- side effects of eval'ing `import _ "./bar"` used addr 1.
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
        -- TODO: Modules defined in multiple files stomp on eachother in the ModuleTable.
        -- , fixtures <> "bar/rab.go"
        ]
