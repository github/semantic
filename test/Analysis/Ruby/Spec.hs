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
      ((res, state), _) <- evaluate "main.rb"
      res `shouldBe` Right [injValue (Value.Integer (Number.Integer 1))]
      Env.names (environment state) `shouldContain` ["foo"]

    it "evaluates load" $ do
      env <- environment . snd . fst <$> evaluate "load.rb"
      Env.names env `shouldContain` ["foo"]

    it "evaluates load with wrapper" $ do
      ((res, state), _) <- evaluate "load-wrap.rb"
      res `shouldBe` Left (SomeExc (injectSum @(EnvironmentError (Value Precise)) (FreeVariable "foo")))
      Env.names (environment state) `shouldContain` [ "Object" ]

    it "evaluates subclass" $ do
      ((res, state), _) <- evaluate "subclass.rb"
      res `shouldBe` Right [injValue (String "\"<bar>\"")]
      Env.names (environment state) `shouldContain` [ "Bar", "Foo" ]

      (derefQName (heap state) ("Bar" :| []) (environment state) >>= deNamespace) `shouldBe` Just ("Bar",  ["baz", "foo", "inspect"])

    it "evaluates modules" $ do
      ((res, state), _) <- evaluate "modules.rb"
      res `shouldBe` Right [injValue (String "\"<hello>\"")]
      Env.names (environment state) `shouldContain` [ "Bar" ]

    it "handles break correctly" $ do
      ((res, _), _) <- evaluate "break.rb"
      res `shouldBe` Right [injValue (Value.Integer (Number.Integer 3))]

    it "handles break correctly" $ do
      ((res, _), _) <- evaluate "next.rb"
      res `shouldBe` Right [injValue (Value.Integer (Number.Integer 8))]

    it "calls functions with arguments" $ do
      ((res, _), _) <- evaluate "call.rb"
      res `shouldBe` Right [injValue (Value.Integer (Number.Integer 579))]

    it "evaluates early return statements" $ do
      ((res, _), _) <- evaluate "early-return.rb"
      res `shouldBe` Right [injValue (Value.Integer (Number.Integer 123))]

    it "has prelude" $ do
      ((res, _), _) <- evaluate "preluded.rb"
      res `shouldBe` Right [injValue (String "\"<foo>\"")]

    it "evaluates __LINE__" $ do
      ((res, _), _) <- evaluate "line.rb"
      res `shouldBe` Right [injValue (Value.Integer (Number.Integer 4))]

    it "resolves builtins used in the prelude" $ do
      ((res, _), traces) <- evaluate "puts.rb"
      res `shouldBe` Right [injValue Unit]
      traces `shouldContain` [ "\"hello\"" ]

  where
    ns n = Just . Latest . Last . Just . injValue . Namespace n
    addr = Address . Precise
    fixtures = "test/fixtures/ruby/analysis/"
    evaluate entry = evalRubyProject (fixtures <> entry)
    evalRubyProject path = testEvaluating <$> evaluateProject rubyParser Language.Ruby rubyPrelude path
