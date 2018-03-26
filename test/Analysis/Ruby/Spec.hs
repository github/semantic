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
      env `shouldBe` [ (name "Object", addr 0)
                     , (name "foo", addr 3) ]

    it "evalutes load" $ do
      env <- findEnv <$> evaluate "load.rb"
      env `shouldBe` [ (name "Object", addr 0)
                     , (name "foo", addr 3) ]

    it "evalutes load with wrapper" $ do
      res <- evaluate "load-wrap.rb"
      findValue res `shouldBe` Left "free variable: \"foo\""
      findEnv res `shouldBe` [ (name "Object", addr 0) ]

    it "evalutes subclass" $ do
      res <- findValue <$> evaluate "subclass.rb"
      res `shouldBe` Right (Right (Right (injValue (String "\"<bar>\""))))

    it "evaluates modules" $ do
      res <- findValue <$> evaluate "modules.rb"
      res `shouldBe` Right (Right (Right (injValue (String "\"<hello>\""))))

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
