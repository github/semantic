{-# LANGUAGE OverloadedLists #-}

module Analysis.Ruby.Spec (spec) where

import Data.Abstract.Value
import Data.Map
import Data.Map.Monoidal as Map

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
      res <- evaluate "subclass.rb"
      findValue res `shouldBe` Right (Right (Right (injValue (String "\"<bar>\""))))
      findEnv res `shouldBe` [ (name "Bar", addr 6)
                             , (name "Foo", addr 3)
                             , (name "Object", addr 0) ]

      let heap = findHeap res
      Map.lookup (Precise 6) heap `shouldBe` ns "Bar" [ (name "baz", addr 8)
                                                      , (name "foo", addr 5)
                                                      , (name "inspect", addr 7) ]

    it "evaluates modules" $ do
      res <- evaluate "modules.rb"
      findValue res `shouldBe` Right (Right (Right (injValue (String "\"<hello>\""))))
      findEnv res `shouldBe` [ (name "Object", addr 0)
                             , (name "Bar", addr 3) ]

    it "has prelude" $ do
      res <- findValue <$> evaluate "preluded.rb"
      res `shouldBe` Right (Right (Right (injValue (String "\"<foo>\""))))

  where
    ns n = Just . Latest . Just . injValue . Namespace (name n)
    addr = Address . Precise
    fixtures = "test/fixtures/ruby/analysis/"
    evaluate entry = evaluateFilesWithPrelude rubyParser
      [ fixtures <> entry
      , fixtures <> "foo.rb"
      ]
