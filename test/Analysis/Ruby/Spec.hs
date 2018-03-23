{-# LANGUAGE OverloadedLists #-}

module Analysis.Ruby.Spec (spec) where

import Data.Abstract.Value
import Data.Map

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evalutes Ruby" $ do
    it "require_relative" $ do
      env <- findEnv <$> evaluate "main.rb"
      env `shouldBe` [ (qualifiedName ["foo"], addr 0) ]

    it "load" $ do
      env <- findEnv <$> evaluate "load.rb"
      env `shouldBe` [ (qualifiedName ["foo"], addr 0) ]

    it "load wrap" $ do
      res <- evaluate "load-wrap.rb"
      findValue res `shouldBe` Left "free variable: \"foo\""
      findEnv res `shouldBe` []

    it "subclass" $ do
      v <- findValue <$> evaluate "subclass.rb"
      v `shouldBe` Right (injValue (String "\"<bar>\""))

  where
    addr = Address . Precise
    fixtures = "test/fixtures/ruby/analysis/"
    evaluate entry = evaluateFiles rubyParser
      [ fixtures <> entry
      , fixtures <> "foo.rb"
      ]
