module Analysis.Ruby.Spec (spec) where

import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable
import Data.Abstract.Value as Value
import Data.Abstract.Number as Number
import Data.AST
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
      (_, (_, Right [(env, res)])) <- evaluate "main.rb"
      res `shouldBe` Value.Integer (Number.Integer 1)
      Env.names env `shouldContain` ["foo"]

    it "evaluates load" $ do
      (_, (_, Right [(env, _)])) <- evaluate "load.rb"
      Env.names env `shouldContain` ["foo"]

    it "evaluates load with wrapper" $ do
      (_, (_, res)) <- evaluate "load-wrap.rb"
      res `shouldBe` Left (SomeExc (inject @(EnvironmentError Precise) (FreeVariable "foo")))

    it "evaluates subclass" $ do
      (_, (state, Right [(env, res)])) <- evaluate "subclass.rb"
      res `shouldBe` String "\"<bar>\""
      Env.names env `shouldContain` [ "Bar", "Foo" ]

      (derefQName (heap state) ("Bar" :| []) env >>= deNamespace) `shouldBe` Just ("Bar",  ["baz", "foo", "inspect"])

    it "evaluates modules" $ do
      (_, (state, Right [(env, res)])) <- evaluate "modules.rb"
      res `shouldBe` String "\"<hello>\""
      Env.names env `shouldContain` [ "Bar" ]

    it "handles break correctly" $ do
      (_, (_, res)) <- evaluate "break.rb"
      fmap snd <$> res `shouldBe` Right [Value.Integer (Number.Integer 3)]

    it "handles break correctly" $ do
      (_, (_, res)) <- evaluate "next.rb"
      fmap snd <$> res `shouldBe` Right [Value.Integer (Number.Integer 8)]

    it "calls functions with arguments" $ do
      (_, (_, res)) <- evaluate "call.rb"
      fmap snd <$> res `shouldBe` Right [Value.Integer (Number.Integer 579)]

    it "evaluates early return statements" $ do
      (_, (_, res)) <- evaluate "early-return.rb"
      fmap snd <$> res `shouldBe` Right [Value.Integer (Number.Integer 123)]

    it "has prelude" $ do
      (_, (_, res)) <- evaluate "preluded.rb"
      fmap snd <$> res `shouldBe` Right [String "\"<foo>\""]

    it "evaluates __LINE__" $ do
      (_, (_, res)) <- evaluate "line.rb"
      fmap snd <$> res `shouldBe` Right [Value.Integer (Number.Integer 4)]

    it "resolves builtins used in the prelude" $ do
      (traces, (_, res)) <- evaluate "puts.rb"
      fmap snd <$> res `shouldBe` Right [Unit]
      traces `shouldContain` [ "\"hello\"" ]

  where
    ns n = Just . Latest . Last . Just . Namespace n
    fixtures = "test/fixtures/ruby/analysis/"
    evaluate entry = evalRubyProject (fixtures <> entry)
    evalRubyProject path = testEvaluating <$> evaluateProject (Proxy :: Proxy 'Language.Ruby) rubyParser Language.Ruby path
