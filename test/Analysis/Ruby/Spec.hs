{-# LANGUAGE OverloadedLists #-}

module Analysis.Ruby.Spec (spec) where

import Data.Abstract.Value
import Data.Map

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evalutes Ruby" $ do
    it "require_relative" $ do
      env <- environment . snd <$> evaluate "main.rb"
      let expectedEnv = [ (qualifiedName ["Object"], addr 0)
                        , (qualifiedName ["foo"], addr 3)]
      env `shouldBe` expectedEnv

    it "load" $ do
      env <- environment . snd <$> evaluate "load.rb"
      let expectedEnv = [ (qualifiedName ["Object"], addr 0)
                        , (qualifiedName ["foo"], addr 3) ]
      env `shouldBe` expectedEnv

    it "load wrap" $ do
      res <- evaluate "load-wrap.rb"
      fst res `shouldBe` Left "free variable: \"foo\""
      environment (snd res) `shouldBe` [(qualifiedName ["Object"], addr 0)]

    it "subclass" $ do
      res <- fst <$> evaluate "subclass.rb"
      res `shouldBe` Right (Right (Right (injValue (String "\"<bar>\""))))

    it "has prelude" $ do
      res <- fst <$> evaluate "preluded.rb"
      res `shouldBe` Right (Right (Right (injValue (String "\"<foo>\""))))

  where
    addr = Address . Precise
    fixtures = "test/fixtures/ruby/analysis/"
    evaluate entry = evaluateFilesWithPrelude rubyParser
      [ fixtures <> entry
      , fixtures <> "foo.rb"
      ]
