{-# LANGUAGE OverloadedLists #-}

module Analysis.Ruby.Spec (spec) where

import Data.Abstract.Evaluatable
import Data.Abstract.Value as Value
import Data.Abstract.Number as Number
import Control.Monad.Effect (SomeExc(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map
import Data.Map.Monoidal as Map
import Data.Sum
import qualified Language.Ruby.Assignment as Ruby
import qualified Data.Language as Language

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "Ruby" $ do
    it "evaluates require_relative" $ do
      env <- environment . snd <$> evaluate "main.rb"
      env `shouldBe` [ ("Object", addr 0)
                     , ("foo", addr 3) ]

    it "evaluates load" $ do
      env <- environment . snd <$> evaluate "load.rb"
      env `shouldBe` [ ("Object", addr 0)
                     , ("foo", addr 3) ]

    it "evaluates load with wrapper" $ do
      res <- evaluate "load-wrap.rb"
      fst res `shouldBe` Left (SomeExc (injectSum @(EnvironmentError (Value Precise)) (FreeVariable "foo")))
      environment (snd res) `shouldBe` [ ("Object", addr 0) ]

    it "evaluates subclass" $ do
      res <- evaluate "subclass.rb"
      fst res `shouldBe` Right [injValue (String "\"<bar>\"")]
      environment (snd res) `shouldBe` [ ("Bar", addr 6)
                                       , ("Foo", addr 3)
                                       , ("Object", addr 0) ]

      heapLookup (Address (Precise 6)) (heap (snd res))
        `shouldBe` ns "Bar" [ ("baz", addr 8)
                            , ("foo", addr 5)
                            , ("inspect", addr 7) ]

    it "evaluates modules" $ do
      res <- evaluate "modules.rb"
      fst res `shouldBe` Right [injValue (String "\"<hello>\"")]
      environment (snd res) `shouldBe` [ ("Object", addr 0)
                                       , ("Bar", addr 3) ]

    it "handles break correctly" $ do
      res <- evaluate "break.rb"
      fst res `shouldBe` Right [injValue (Value.Integer (Number.Integer 3))]

    it "handles break correctly" $ do
      res <- evaluate "next.rb"
      fst res `shouldBe` Right [injValue (Value.Integer (Number.Integer 8))]

    it "calls functions with arguments" $ do
      res <- evaluate "call.rb"
      fst res `shouldBe` Right [injValue (Value.Integer (Number.Integer 579))]

    it "evaluates early return statements" $ do
      res <- evaluate "early-return.rb"
      fst res `shouldBe` Right [injValue (Value.Integer (Number.Integer 123))]

    it "has prelude" $ do
      res <- fst <$> evaluate "preluded.rb"
      res `shouldBe` Right [injValue (String "\"<foo>\"")]

  where
    ns n = Just . Latest . Just . injValue . Namespace n
    addr = Address . Precise
    fixtures = "test/fixtures/ruby/analysis/"
    evaluate entry = evalRubyProject (fixtures <> entry)
    evalRubyProject path = testEvaluating <$> evaluateProject rubyParser Language.Ruby rubyPrelude path
