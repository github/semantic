{-# LANGUAGE TypeApplications #-}
module Analysis.Ruby.Spec (spec) where

import Data.Abstract.Value
import Data.Map

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evalutes Ruby" $ do
    it "require_relative" $ do
      env <- evaluate "main.rb"
      let expectedEnv = Environment $ fromList [ (qualifiedName ["foo"], addr 0) ]
      env `shouldBe` expectedEnv

  where
    addr = Address . Precise
    fixtures = "test/fixtures/ruby/analysis/"
    evaluate entry = snd . fst . fst . fst <$>
      evaluateFiles @RubyValue rubyParser
      [ fixtures <> entry
      , fixtures <> "foo.rb"
      ]
