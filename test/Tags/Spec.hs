{-# LANGUAGE OverloadedStrings #-}
module Tags.Spec (spec) where

import Control.Effect.Reader
import Semantic.Api.Symbols
import Source.Loc
import SpecHelpers
import qualified System.Path as Path
import Tags.Tagging as Tags

spec :: Spec
spec = do
  describe "go" $ do
    it "produces tags for functions with docs" $
      parseTestFile [Function] (Path.relFile "test/fixtures/go/tags/simple_functions.go") `shouldReturn`
        [ Tag "TestFromBits" Function (Loc (Range 51 92) (Span (Pos 6 1) (Pos 8 2))) "func TestFromBits(t *testing.T) {" (Just "// TestFromBits ...")
        , Tag "Hi" Function (Loc (Range 94 107) (Span (Pos 10 1) (Pos 11 2))) "func Hi()" Nothing ]

    it "produces tags for methods" $
      parseTestFile [Method] (Path.relFile "test/fixtures/go/tags/method.go") `shouldReturn`
        [ Tag "CheckAuth" Method (Loc (Range 19 118) (Span (Pos 3 1) (Pos 3 100))) "func (c *apiClient) CheckAuth(req *http.Request, user, repo string) (*authenticatedActor, error)" Nothing]

    it "produces tags for calls" $
      parseTestFile [Call] (Path.relFile "test/fixtures/go/tags/simple_functions.go") `shouldReturn`
        [ Tag "Hi" Call (Loc (Range 86 90) (Span (Pos 7 2) (Pos 7 6))) "Hi()" Nothing]

  describe "javascript and typescript" $ do
    it "produces tags for functions with docs" $
      parseTestFile [Function] (Path.relFile "test/fixtures/javascript/tags/simple_function_with_docs.js") `shouldReturn`
        [ Tag "myFunction" Function (Loc (Range 22 59) (Span (Pos 2 1) (Pos 4 2))) "function myFunction()" (Just "// This is myFunction") ]

    it "produces tags for classes" $
      parseTestFile [Class] (Path.relFile "test/fixtures/typescript/tags/class.ts") `shouldReturn`
        [ Tag "FooBar" Class (Loc (Range 0 15) (Span (Pos 1 1) (Pos 1 16))) "class FooBar" Nothing ]

    it "produces tags for modules" $
      parseTestFile [Tags.Module] (Path.relFile "test/fixtures/typescript/tags/module.ts") `shouldReturn`
        [ Tag "APromise" Tags.Module (Loc (Range 0 19) (Span (Pos 1 1) (Pos 1 20))) "module APromise { }" Nothing ]

  describe "python" $ do
    it "produces tags for functions" $
      parseTestFile [Function] (Path.relFile "test/fixtures/python/tags/simple_functions.py") `shouldReturn`
        [ Tag "Foo" Function (Loc (Range 0 68) (Span (Pos 1 1) (Pos 5 17))) "def Foo(x):" Nothing
        , Tag "Bar" Function (Loc (Range 70 136) (Span (Pos 7 1) (Pos 11 13))) "def Bar():" Nothing
        , Tag "local" Function (Loc (Range 85 114) (Span (Pos 8 5) (Pos 9 17))) "def local():" Nothing
        ]

    it "produces tags for functions with docs" $
      parseTestFile [Function] (Path.relFile "test/fixtures/python/tags/simple_function_with_docs.py") `shouldReturn`
        [ Tag "Foo" Function (Loc (Range 0 59) (Span (Pos 1 1) (Pos 3 13))) "def Foo(x):" (Just "\"\"\"This is the foo function\"\"\"") ]

    it "produces tags for classes" $
      parseTestFile [Class, Function] (Path.relFile "test/fixtures/python/tags/class.py") `shouldReturn`
        [ Tag "Foo" Class (Loc (Range 0 95) (Span (Pos 1 1) (Pos 5 17))) "class Foo:" (Just "\"\"\"The Foo class\"\"\"")
        , Tag "f" Function (Loc (Range 39 95) (Span (Pos 3 5) (Pos 5 17))) "def f(self):" (Just "\"\"\"The f method\"\"\"")
        ]

    it "produces tags for multi-line functions" $
      parseTestFile [Function] (Path.relFile "test/fixtures/python/tags/multiline.py") `shouldReturn`
        [ Tag "Foo" Function (Loc (Range 0 29) (Span (Pos 1 1) (Pos 3 13))) "def Foo(x," Nothing ]

  describe "ruby" $ do
    it "produces tags for methods" $
      parseTestFile [Method] (Path.relFile "test/fixtures/ruby/tags/simple_method.rb") `shouldReturn`
        [ Tag "foo" Method (Loc (Range 0 31) (Span (Pos 1 1) (Pos 4 4))) "def foo" Nothing ]

    it "produces tags for sends" $
      parseTestFile [Call] (Path.relFile "test/fixtures/ruby/tags/simple_method.rb") `shouldReturn`
        [ Tag "puts" Call (Loc (Range 10 19) (Span (Pos 2 3) (Pos 2 12))) "puts \"hi\"" Nothing
        , Tag "bar" Call (Loc (Range 22 27) (Span (Pos 3 3) (Pos 3 8))) "a.bar" Nothing
        , Tag "a" Call (Loc (Range 22 23) (Span (Pos 3 3) (Pos 3 4))) "a" Nothing
        ]

    it "produces tags for methods with docs" $
      parseTestFile [Method] (Path.relFile "test/fixtures/ruby/tags/simple_method_with_docs.rb") `shouldReturn`
        [ Tag "foo" Method (Loc (Range 14 25) (Span (Pos 2 1) (Pos 3 4))) "def foo" (Just "# Public: foo") ]

    it "correctly tags files containing multibyte UTF-8 characters" $
      parseTestFile [Method] (Path.relFile "test/fixtures/ruby/tags/unicode_identifiers.rb") `shouldReturn`
        [ Tag "日本語" Method (Loc (Range 16 43) (Span (Pos 2 1) (Pos 4 4))) "def 日本語" (Just "# coding: utf-8")]

    it "produces tags for methods and classes with docs" $
      parseTestFile [Class, Method, Tags.Module] (Path.relFile "test/fixtures/ruby/tags/class_module.rb") `shouldReturn`
        [ Tag "Foo" Tags.Module (Loc (Range 14 118) (Span (Pos 2 1 ) (Pos 12 4))) "module Foo" (Just "# Public: Foo")
        , Tag "Bar" Class  (Loc (Range 44 114) (Span (Pos 5 3 ) (Pos 11 6))) "class Bar" (Just "# Public: Bar")
        , Tag "baz" Method (Loc (Range 77 108) (Span (Pos 8 5 ) (Pos 10 8))) "def baz(a)" (Just "# Public: baz")
        , Tag "C" Class (Loc (Range 120 188) (Span (Pos 14 1) (Pos 20 4))) "class A::B::C" Nothing
        , Tag "foo" Method (Loc (Range 136 163) (Span (Pos 15 3) (Pos 17 6))) "def foo" Nothing
        , Tag "foo" Method (Loc (Range 166 184) (Span (Pos 18 3) (Pos 19 6))) "def self.foo" Nothing
        ]

parseTestFile :: Foldable t => t Tags.Kind -> Path.RelFile -> IO [Tag]
parseTestFile include path = runTaskOrDie $ readBlob (fileForPath (Path.toString path)) >>= runReader defaultLanguageModes . fmap (filter ((`elem` include) . kind)) . tagsForBlob
