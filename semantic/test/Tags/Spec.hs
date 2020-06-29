{-# LANGUAGE OverloadedStrings #-}
module Tags.Spec (spec) where

import qualified Analysis.File as File
import Proto.Semantic as P
import Semantic.Api.Symbols
import Source.Loc
import SpecHelpers
import qualified System.Path as Path
import Tags.Tagging.Precise

spec :: Spec
spec = do
  describe "go" $ do
    it "produces tags for functions with docs (TODO)" $
      parseTestFile [P.FUNCTION] (Path.relFile "semantic/test/fixtures/go/tags/simple_functions.go") `shouldReturn`
        [ Tag "TestFromBits" P.FUNCTION P.DEFINITION (OneIndexedSpan (Span (Pos 6 6) (Pos 6 18))) "func TestFromBits(t *testing.T) {" (UTF16CodeUnitSpan (Span (Pos 5 5) (Pos 5 17)))
        , Tag "Hi" P.FUNCTION P.DEFINITION (OneIndexedSpan (Span (Pos 10 6) (Pos 10 8))) "func Hi() {" (UTF16CodeUnitSpan (Span (Pos 9 5) (Pos 9 7)))
        ]

    it "produces tags for methods" $
      parseTestFile [] (Path.relFile "semantic/test/fixtures/go/tags/method.go") `shouldReturn`
        [ Tag "CheckAuth" P.METHOD P.DEFINITION (OneIndexedSpan (Span (Pos 3 21) (Pos 3 30))) "func (c *apiClient) CheckAuth(req *http.Request, user, repo string) (*authenticatedActor, error) {}" (UTF16CodeUnitSpan (Span (Pos 2 20) (Pos 2 29)))
        ]

    it "produces tags for calls" $
      parseTestFile [P.CALL] (Path.relFile "semantic/test/fixtures/go/tags/simple_functions.go") `shouldReturn`
        [ Tag "Hi" P.CALL P.REFERENCE (OneIndexedSpan (Span (Pos 7 2) (Pos 7 4))) "Hi()" (UTF16CodeUnitSpan (Span (Pos 6 1) (Pos 6 3)))
        ]

  describe "javascript and typescript" $ do
    it "produces tags for functions with docs (TODO)" $
      parseTestFile [] (Path.relFile "semantic/test/fixtures/javascript/tags/simple_function_with_docs.js") `shouldReturn`
        [ Tag "myFunction" P.FUNCTION P.DEFINITION (OneIndexedSpan (Span (Pos 2 10) (Pos 2 20))) "function myFunction() {" (UTF16CodeUnitSpan (Span (Pos 1 9) (Pos 1 19)))
        ]

    it "produces tags for classes" $
      parseTestFile [] (Path.relFile "semantic/test/fixtures/typescript/tags/class.ts") `shouldReturn`
        [ Tag "FooBar" P.CLASS P.DEFINITION (OneIndexedSpan (Span (Pos 1 7) (Pos 1 13))) "class FooBar {}" (UTF16CodeUnitSpan (Span (Pos 0 6) (Pos 0 12)))
        ]

    it "produces tags for modules" $
      parseTestFile [] (Path.relFile "semantic/test/fixtures/typescript/tags/module.ts") `shouldReturn`
        [ Tag "APromise" P.MODULE P.DEFINITION (OneIndexedSpan (Span (Pos 1 8) (Pos 1 16))) "module APromise { }" (UTF16CodeUnitSpan (Span (Pos 0 7) (Pos 0 15)))
        ]

  describe "python" $ do
    it "produces tags for functions" $
      parseTestFile [] (Path.relFile "semantic/test/fixtures/python/tags/simple_functions.py") `shouldReturn`
        [ Tag "Foo" P.FUNCTION P.DEFINITION (OneIndexedSpan (Span (Pos 1 5) (Pos 1 8))) "def Foo(x):" (UTF16CodeUnitSpan (Span (Pos 0 4) (Pos 0 7)))
        , Tag "Bar" P.FUNCTION P.DEFINITION (OneIndexedSpan (Span (Pos 7 5) (Pos 7 8))) "def Bar():" (UTF16CodeUnitSpan (Span (Pos 6 4) (Pos 6 7)))
        , Tag "local" P.FUNCTION P.DEFINITION (OneIndexedSpan (Span (Pos 8 9) (Pos 8 14))) "def local():" (UTF16CodeUnitSpan (Span (Pos 7 8) (Pos 7 13)))
        ]

    it "produces tags for functions with docs" $
      parseTestFile [] (Path.relFile "semantic/test/fixtures/python/tags/simple_function_with_docs.py") `shouldReturn`
        [ Tag "Foo" P.FUNCTION P.DEFINITION (OneIndexedSpan (Span (Pos 1 5) (Pos 1 8))) "def Foo(x):" (UTF16CodeUnitSpan (Span (Pos 0 4) (Pos 0 7)))
        ]

    it "produces tags for classes" $
      parseTestFile [] (Path.relFile "semantic/test/fixtures/python/tags/class.py") `shouldReturn`
        [ Tag "Foo" P.CLASS P.DEFINITION (OneIndexedSpan (Span (Pos 1 7) (Pos 1 10))) "class Foo:" (UTF16CodeUnitSpan (Span (Pos 0 6) (Pos 0 9)))
        , Tag "f" P.FUNCTION P.DEFINITION (OneIndexedSpan (Span (Pos 3 9) (Pos 3 10))) "def f(self):" (UTF16CodeUnitSpan (Span (Pos 2 8) (Pos 2 9)))
        ]

    it "produces tags for multi-line functions" $
      parseTestFile [P.FUNCTION] (Path.relFile "semantic/test/fixtures/python/tags/multiline.py") `shouldReturn`
        [ Tag "Foo" P.FUNCTION P.DEFINITION (OneIndexedSpan (Span (Pos 1 5) (Pos 1 8))) "def Foo(x," (UTF16CodeUnitSpan (Span (Pos 0 4) (Pos 0 7)))
        ]

  describe "ruby" $ do
    it "produces tags for methods" $
      parseTestFile [P.METHOD] (Path.relFile "semantic/test/fixtures/ruby/tags/simple_method.rb") `shouldReturn`
        [ Tag "foo" P.METHOD P.DEFINITION (OneIndexedSpan (Span (Pos 1 5) (Pos 1 8))) "def foo" (UTF16CodeUnitSpan (Span (Pos 0 4) (Pos 0 7)))
        ]

    it "produces tags for sends" $
      parseTestFile [P.CALL] (Path.relFile "semantic/test/fixtures/ruby/tags/simple_method.rb") `shouldReturn`
        [ Tag "puts" P.CALL P.REFERENCE (OneIndexedSpan (Span (Pos 2 3) (Pos 2 7))) "puts \"hi\"" (UTF16CodeUnitSpan (Span (Pos 1 2) (Pos 1 6)))
        , Tag "bar" P.CALL P.REFERENCE (OneIndexedSpan (Span (Pos 3 5) (Pos 3 8))) "a.bar" (UTF16CodeUnitSpan (Span (Pos 2 4) (Pos 2 7)))
        , Tag "a" P.CALL P.REFERENCE (OneIndexedSpan (Span (Pos 3 3) (Pos 3 4))) "a.bar" (UTF16CodeUnitSpan (Span (Pos 2 2) (Pos 2 3)))
        ]

    it "produces tags for methods with docs (TODO)" $
      parseTestFile [] (Path.relFile "semantic/test/fixtures/ruby/tags/simple_method_with_docs.rb") `shouldReturn`
        [ Tag "foo" P.METHOD P.DEFINITION (OneIndexedSpan (Span (Pos 2 5) (Pos 2 8))) "def foo" (UTF16CodeUnitSpan (Span (Pos 1 4) (Pos 1 7)))
        ]

    it "correctly tags files containing multibyte UTF-8 characters (TODO)" $
      parseTestFile [] (Path.relFile "semantic/test/fixtures/ruby/tags/unicode_identifiers.rb") `shouldReturn`
        [ Tag "日本語" P.METHOD P.DEFINITION (OneIndexedSpan (Span (Pos 2 5) (Pos 2 14))) "def 日本語" (UTF16CodeUnitSpan (Span (Pos 1 4) (Pos 1 7)))
        ]

    it "produces tags for methods and classes with docs (TODO)" $
      parseTestFile [P.MODULE, P.CLASS, P.METHOD] (Path.relFile "semantic/test/fixtures/ruby/tags/class_module.rb") `shouldReturn`
        [ Tag "Foo" P.MODULE P.DEFINITION (OneIndexedSpan (Span (Pos 2 8) (Pos 2 11))) "module Foo" (UTF16CodeUnitSpan (Span (Pos 1 7) (Pos 1 10)))
        , Tag "Bar" P.CLASS P.DEFINITION (OneIndexedSpan  (Span (Pos 5 9) (Pos 5 12))) "class Bar" (UTF16CodeUnitSpan (Span (Pos 4 8) (Pos 4 11)))
        , Tag "baz" P.METHOD P.DEFINITION (OneIndexedSpan (Span (Pos 8 9) (Pos 8 12))) "def baz(a)" (UTF16CodeUnitSpan (Span (Pos 7 8) (Pos 7 11)))
        , Tag "C" P.CLASS P.DEFINITION (OneIndexedSpan (Span (Pos 14 13) (Pos 14 14))) "class A::B::C" (UTF16CodeUnitSpan (Span (Pos 13 12) (Pos 13 13)))
        , Tag "foo" P.METHOD P.DEFINITION (OneIndexedSpan (Span (Pos 15 7) (Pos 15 10))) "def foo" (UTF16CodeUnitSpan (Span (Pos 14 6) (Pos 14 9)))
        , Tag "foo" P.METHOD P.DEFINITION (OneIndexedSpan (Span (Pos 18 12) (Pos 18 15))) "def self.foo" (UTF16CodeUnitSpan (Span (Pos 17 11) (Pos 17 14)))
        ]

parseTestFile :: Foldable t => t P.SyntaxType -> Path.RelFile -> IO [Tag]
parseTestFile include path = runTaskOrDie $ readBlob (File.fromPath path) >>= fmap (filter only) . tagsForBlob
  where only t = null include || (`elem` include) (tagSyntaxType t)
