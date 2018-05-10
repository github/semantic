{-# LANGUAGE OverloadedLists #-}

module Analysis.Ruby.Spec (spec) where

import Data.Abstract.Environment as Env
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
      (res, state) <- evaluate "main.rb"
      res `shouldBe` Right [injValue (Value.Integer (Number.Integer 1))]
      Env.names (environment state) `shouldContain` ["foo"]

    it "evaluates load" $ do
      env <- environment . snd <$> evaluate "load.rb"
      Env.names env `shouldContain` ["foo"]

    it "evaluates load with wrapper" $ do
      res <- evaluate "load-wrap.rb"
      fst res `shouldBe` Left (SomeExc (injectSum @(EnvironmentError (Value Precise)) (FreeVariable "foo")))
      Env.names (environment (snd res)) `shouldContain` [ "Object" ]

    it "evaluates subclass" $ do
      res <- evaluate "subclass.rb"
      fst res `shouldBe` Right [injValue (String "\"<bar>\"")]
      Env.names (environment (snd res)) `shouldContain` [ "Bar", "Foo" ]

      (derefQName (heap (snd res)) ("Bar" :| []) (environment (snd res)) >>= deNamespace) `shouldBe` Just ("Bar",  ["baz", "foo", "inspect"])

    it "evaluates modules" $ do
      res <- evaluate "modules.rb"
      fst res `shouldBe` Right [injValue (String "\"<hello>\"")]
      Env.names (environment (snd res)) `shouldContain` [ "Bar" ]

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

    it "evaluates __LINE__" $ do
      res <- fst <$> evaluate "line.rb"
      res `shouldBe` Right [injValue (Value.Integer (Number.Integer 4))]

  where
    ns n = Just . Latest . Just . injValue . Namespace n
    addr = Address . Precise
    fixtures = "test/fixtures/ruby/analysis/"
    evaluate entry = evalRubyProject (fixtures <> entry)
    evalRubyProject path = testEvaluating <$> evaluateProject rubyParser Language.Ruby rubyPrelude path
