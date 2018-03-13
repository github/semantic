{-# LANGUAGE TypeApplications #-}
module Analysis.Python.Spec (spec) where

import Data.Abstract.Value
import Data.Map

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evalutes Python" $ do
    it "imports" $ do
      res <- evaluate "main.py"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["a", "foo"], addr 0)
            , (qualifiedName ["b", "c", "baz"], addr 1)
            ]
      assertEnvironment res expectedEnv

    it "imports with aliases" $ do
      res <- evaluate "main1.py"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["b", "foo"], addr 0)
            , (qualifiedName ["e", "baz"], addr 1)
            ]
      assertEnvironment res expectedEnv

    it "imports using 'from' syntax" $ do
      res <- evaluate "main2.py"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["foo"], addr 0)
            , (qualifiedName ["bar"], addr 1)
            ]
      assertEnvironment res expectedEnv

  where
    assertEnvironment result expectedEnv = case result of
      Left e -> expectationFailure ("Evaluating expected to succeed, but failed with: " <> e)
      Right res -> let Just (Interface _ env) = prjValue @(Interface Precise) res in env `shouldBe` expectedEnv

    addr = Address . Precise
    fixtures = "test/fixtures/python/analysis/"
    evaluate entry = fst . fst . fst . fst <$>
      evaluateFiles @PythonValue pythonParser
      [ fixtures <> entry
      , fixtures <> "a.py"
      , fixtures <> "b/c.py"
      ]
