{-# LANGUAGE OverloadedLists #-}

module Analysis.Ruby.Spec (spec) where

import Data.Abstract.Value
import Data.Map

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "Ruby" $ do
    it "evaluates require_relative" $ do
      env <- findEnv <$> evaluate "main.rb"
      let expectedEnv = [ (qualifiedName ["Object"], addr 0)
                        , (qualifiedName ["foo"], addr 3)]
      env `shouldBe` expectedEnv

    it "evalutes load" $ do
      env <- findEnv <$> evaluate "load.rb"
      let expectedEnv = [ (qualifiedName ["Object"], addr 0)
                        , (qualifiedName ["foo"], addr 3) ]
      env `shouldBe` expectedEnv

    it "load wrap" $ do
      res <- evaluate "load-wrap.rb"
      findValue res `shouldBe` Left "free variable: \"foo\""
      findEnv res `shouldBe` [(qualifiedName ["Object"], addr 0)]

    it "subclass" $ do
      res <- findValue <$> evaluate "subclass.rb"
      res `shouldBe` Right (Right (Right (injValue (String "\"<bar>\""))))

    it "has prelude" $ do
      res <- findValue <$> evaluate "preluded.rb"
      res `shouldBe` Right (Right (Right (injValue (String "\"<foo>\""))))

  where
    addr = Address . Precise
    fixtures = "test/fixtures/ruby/analysis/"
    evaluate entry = evaluateFilesWithPrelude rubyParser
      [ fixtures <> entry
      , fixtures <> "foo.rb"
      ]
