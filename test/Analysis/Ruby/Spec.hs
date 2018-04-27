{-# LANGUAGE OverloadedLists #-}

module Analysis.Ruby.Spec (spec) where

import Data.Abstract.Evaluatable
import Data.Abstract.Value as Value
import Data.Abstract.Number as Number
import Control.Monad.Effect (SomeExc(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map
import Data.Map.Monoidal as Map
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
      fst res `shouldBe` Right (Right (Right (Right (Right (Left (SomeExc (FreeVariableError "foo")))))))
      environment (snd res) `shouldBe` [ ("Object", addr 0) ]

    it "evaluates subclass" $ do
      res <- evaluate "subclass.rb"
      fst res `shouldBe` Right (Right (Right (Right (Right (Right (Right (pure (injValue (String "\"<bar>\"")))))))))
      environment (snd res) `shouldBe` [ ("Bar", addr 6)
                                       , ("Foo", addr 3)
                                       , ("Object", addr 0) ]

      heapLookup (Address (Precise 6)) (heap (snd res))
        `shouldBe` ns "Bar" [ ("baz", addr 8)
                            , ("foo", addr 5)
                            , ("inspect", addr 7) ]

    it "evaluates modules" $ do
      res <- evaluate "modules.rb"
      fst res `shouldBe` Right (Right (Right (Right (Right (Right (Right (pure (injValue (String "\"<hello>\"")))))))))
      environment (snd res) `shouldBe` [ ("Object", addr 0)
                                       , ("Bar", addr 3) ]

    it "handles break correctly" $ do
      res <- evaluate "break.rb"
      fst res `shouldBe` Right (Right (Right (Right (Right (Right (Right (pure (injValue (Value.Integer (Number.Integer 3))))))))))

    it "handles break correctly" $ do
      res <- evaluate "next.rb"
      fst res `shouldBe` Right (Right (Right (Right (Right (Right (Right (pure (injValue (Value.Integer (Number.Integer 8))))))))))

    it "evaluates early return statements" $ do
      res <- evaluate "early-return.rb"
      fst res `shouldBe` Right (Right (Right (Right (Right (Right (Right (pure (injValue (Value.Integer (Number.Integer 123))))))))))

    it "has prelude" $ do
      res <- fst <$> evaluate "preluded.rb"
      res `shouldBe` Right (Right (Right (Right (Right (Right (Right (pure (injValue (String "\"<foo>\"")))))))))

  where
    ns n = Just . Latest . Just . injValue . Namespace n
    addr = Address . Precise
    fixtures = "test/fixtures/ruby/analysis/"
    evaluate entry = evalRubyProject (fixtures <> entry)
    evalRubyProject path = interpret @(TestEvaluating Ruby.Term) <$> evaluateProject rubyParser Language.Ruby rubyPrelude path
