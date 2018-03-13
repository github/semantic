{-# LANGUAGE TypeApplications #-}
module Analysis.Ruby.Spec (spec) where

import Data.Abstract.Value
import Data.Map

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evalutes Ruby" $ do
    it "require_relatives" $ do
      res <- evaluate "main.rb"
      let expectedEnv = Environment $ fromList [ (qualifiedName ["foo"], addr 0) ]
      assertEnvironment res expectedEnv

  where
    assertEnvironment result expectedEnv = case result of
      Left e -> expectationFailure ("Evaluating expected to succeed, but failed with: " <> e)
      Right res -> let Just (Interface _ env) = prjValue @(Interface Precise) res in env `shouldBe` expectedEnv

    addr = Address . Precise
    fixtures = "test/fixtures/ruby/analysis/"
    evaluate entry = fst . fst . fst . fst <$>
      evaluateFiles @RubyValue rubyParser
      [ fixtures <> entry
      , fixtures <> "foo.rb"
      ]
