{-# LANGUAGE OverloadedLists #-}

module Analysis.Ruby.Spec (spec) where

import Data.Abstract.Evaluatable (EvalError(..))
import Data.Abstract.Value
import Control.Monad.Effect (SomeExc(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map
import Data.Map.Monoidal as Map

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "Ruby" $ do
    it "evaluates require_relative" $ do
      env <- environment . snd <$> evaluate "main.rb"
      env `shouldBe` [ (name "Object", addr 0)
                     , (name "foo", addr 3) ]

    it "evaluates load" $ do
      env <- environment . snd <$> evaluate "load.rb"
      env `shouldBe` [ (name "Object", addr 0)
                     , (name "foo", addr 3) ]

    it "evaluates load with wrapper" $ do
      res <- evaluate "load-wrap.rb"
      fst res `shouldBe` Right (Right (Right (Right (Left (SomeExc (FreeVariableError ("foo" :| [])))))))
      environment (snd res) `shouldBe` [ (name "Object", addr 0) ]

    it "evaluates subclass" $ do
      res <- evaluate "subclass.rb"
      fst res `shouldBe` Right (Right (Right (Right (Right (injValue (String "\"<bar>\""))))))
      environment (snd res) `shouldBe` [ (name "Bar", addr 6)
                                       , (name "Foo", addr 3)
                                       , (name "Object", addr 0) ]

      heapLookup (Address (Precise 6)) (heap (snd res))
        `shouldBe` ns "Bar" [ (name "baz", addr 8)
                            , (name "foo", addr 5)
                            , (name "inspect", addr 7) ]

    it "evaluates modules" $ do
      res <- evaluate "modules.rb"
      fst res `shouldBe` Right (Right (Right (Right (Right (injValue (String "\"<hello>\""))))))
      environment (snd res) `shouldBe` [ (name "Object", addr 0)
                                       , (name "Bar", addr 3) ]

    it "has prelude" $ do
      res <- fst <$> evaluate "preluded.rb"
      res `shouldBe` Right (Right (Right (Right (Right (injValue (String "\"<foo>\""))))))

  where
    ns n = Just . Latest . Just . injValue . Namespace (name n)
    addr = Address . Precise
    fixtures = "test/fixtures/ruby/analysis/"
    evaluate entry = evaluateFilesWithPrelude rubyParser
      [ fixtures <> entry
      , fixtures <> "foo.rb"
      ]
