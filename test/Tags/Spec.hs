module Tags.Spec (spec) where

import Data.Text (Text)
import SpecHelpers
import Tags.Tagging as Tags
import qualified System.Path as Path

spec :: Spec
spec = do
  describe "go" $ do
    it "produces tags for functions with docs" $ do
      (blob, tree) <- parseTestFile goParser (Path.relFile "test/fixtures/go/tags/simple_functions.go")
      runTagging (blobLanguage blob) symbolsToSummarize (blobSource blob) tree `shouldBe`
        [ Tag "TestFromBits" Function (Span (Pos 6 1) (Pos 8 2)) "func TestFromBits(t *testing.T) {" (Just "// TestFromBits ...")
        , Tag "Hi" Function (Span (Pos 10 1) (Pos 11 2)) "func Hi()" Nothing ]

    it "produces tags for methods" $ do
      (blob, tree) <- parseTestFile goParser (Path.relFile "test/fixtures/go/tags/method.go")
      runTagging (blobLanguage blob) symbolsToSummarize (blobSource blob) tree `shouldBe`
        [ Tag "CheckAuth" Method (Span (Pos 3 1) (Pos 3 100)) "func (c *apiClient) CheckAuth(req *http.Request, user, repo string) (*authenticatedActor, error)" Nothing]

    it "produces tags for calls" $ do
      (blob, tree) <- parseTestFile goParser (Path.relFile "test/fixtures/go/tags/simple_functions.go")
      runTagging (blobLanguage blob) ["Call"] (blobSource blob) tree `shouldBe`
        [ Tag "Hi" Call (Span (Pos 7 2) (Pos 7 6)) "Hi()" Nothing]

  describe "javascript and typescript" $ do
    it "produces tags for functions with docs" $ do
      (blob, tree) <- parseTestFile typescriptParser (Path.relFile "test/fixtures/javascript/tags/simple_function_with_docs.js")
      runTagging (blobLanguage blob) symbolsToSummarize (blobSource blob) tree `shouldBe`
        [ Tag "myFunction" Function (Span (Pos 2 1) (Pos 4 2)) "function myFunction()" (Just "// This is myFunction") ]

    it "produces tags for classes" $ do
      (blob, tree) <- parseTestFile typescriptParser (Path.relFile "test/fixtures/typescript/tags/class.ts")
      runTagging (blobLanguage blob) symbolsToSummarize (blobSource blob) tree `shouldBe`
        [ Tag "FooBar" Class (Span (Pos 1 1) (Pos 1 16)) "class FooBar" Nothing ]

    it "produces tags for modules" $ do
      (blob, tree) <- parseTestFile typescriptParser (Path.relFile "test/fixtures/typescript/tags/module.ts")
      runTagging (blobLanguage blob) symbolsToSummarize (blobSource blob) tree `shouldBe`
        [ Tag "APromise" Tags.Module (Span (Pos 1 1) (Pos 1 20)) "module APromise { }" Nothing ]

  describe "python" $ do
    it "produces tags for functions" $ do
      (blob, tree) <- parseTestFile pythonParser (Path.relFile "test/fixtures/python/tags/simple_functions.py")
      runTagging (blobLanguage blob) symbolsToSummarize (blobSource blob) tree `shouldBe`
        [ Tag "Foo" Function (Span (Pos 1 1) (Pos 5 17)) "def Foo(x):" Nothing
        , Tag "Bar" Function (Span (Pos 7 1) (Pos 11 13)) "def Bar():" Nothing
        , Tag "local" Function (Span (Pos 8 5) (Pos 9 17)) "def local():" Nothing
        ]

    it "produces tags for functions with docs" $ do
      (blob, tree) <- parseTestFile pythonParser (Path.relFile "test/fixtures/python/tags/simple_function_with_docs.py")
      runTagging (blobLanguage blob) symbolsToSummarize (blobSource blob) tree `shouldBe`
        [ Tag "Foo" Function (Span (Pos 1 1) (Pos 3 13)) "def Foo(x):" (Just "\"\"\"This is the foo function\"\"\"") ]

    it "produces tags for classes" $ do
      (blob, tree) <- parseTestFile pythonParser (Path.relFile "test/fixtures/python/tags/class.py")
      runTagging (blobLanguage blob) symbolsToSummarize (blobSource blob) tree `shouldBe`
        [ Tag "Foo" Class (Span (Pos 1 1) (Pos 5 17)) "class Foo:" (Just "\"\"\"The Foo class\"\"\"")
        , Tag "f" Function (Span (Pos 3 5) (Pos 5 17)) "def f(self):" (Just "\"\"\"The f method\"\"\"")
        ]

    it "produces tags for multi-line functions" $ do
      (blob, tree) <- parseTestFile pythonParser (Path.relFile "test/fixtures/python/tags/multiline.py")
      runTagging (blobLanguage blob) symbolsToSummarize (blobSource blob) tree `shouldBe`
        [ Tag "Foo" Function (Span (Pos 1 1) (Pos 3 13)) "def Foo(x," Nothing ]

  describe "ruby" $ do
    it "produces tags for methods" $ do
      (blob, tree) <- parseTestFile rubyParser (Path.relFile "test/fixtures/ruby/tags/simple_method.rb")
      runTagging (blobLanguage blob) symbolsToSummarize (blobSource blob) tree `shouldBe`
        [ Tag "foo" Method (Span (Pos 1 1) (Pos 4 4)) "def foo" Nothing ]

    it "produces tags for sends" $ do
      (blob, tree) <- parseTestFile rubyParser (Path.relFile "test/fixtures/ruby/tags/simple_method.rb")
      runTagging (blobLanguage blob) ["Send"] (blobSource blob) tree `shouldBe`
        [ Tag "puts" Call (Span (Pos 2 3) (Pos 2 12)) "puts \"hi\"" Nothing
        , Tag "bar" Call (Span (Pos 3 3) (Pos 3 8)) "a.bar" Nothing
        , Tag "a" Call (Span (Pos 3 3) (Pos 3 4)) "a" Nothing
        ]

    it "produces tags for methods with docs" $ do
      (blob, tree) <- parseTestFile rubyParser (Path.relFile "test/fixtures/ruby/tags/simple_method_with_docs.rb")
      runTagging (blobLanguage blob) symbolsToSummarize (blobSource blob) tree `shouldBe`
        [ Tag "foo" Method (Span (Pos 2 1) (Pos 3 4)) "def foo" (Just "# Public: foo") ]

    it "correctly tags files containing multibyte UTF-8 characters" $ do
      (blob, tree) <- parseTestFile rubyParser (Path.relFile "test/fixtures/ruby/tags/unicode_identifiers.rb")
      runTagging (blobLanguage blob) symbolsToSummarize (blobSource blob) tree `shouldBe`
        [ Tag "日本語" Method (Span (Pos 2 1) (Pos 4 4)) "def 日本語" (Just "# coding: utf-8")]

    it "produces tags for methods and classes with docs" $ do
      (blob, tree) <- parseTestFile rubyParser (Path.relFile "test/fixtures/ruby/tags/class_module.rb")
      runTagging (blobLanguage blob) symbolsToSummarize (blobSource blob) tree `shouldBe`
        [ Tag "Foo" Tags.Module (Span (Pos 2 1 ) (Pos 12 4)) "module Foo" (Just "# Public: Foo")
        , Tag "Bar" Class  (Span (Pos 5 3 ) (Pos 11 6)) "class Bar" (Just "# Public: Bar")
        , Tag "baz" Method (Span (Pos 8 5 ) (Pos 10 8)) "def baz(a)" (Just "# Public: baz")
        , Tag "C" Class (Span (Pos 14 1) (Pos 20 4)) "class A::B::C" Nothing
        , Tag "foo" Method (Span (Pos 15 3) (Pos 17 6)) "def foo" Nothing
        , Tag "foo" Method (Span (Pos 18 3) (Pos 19 6)) "def self.foo" Nothing
        ]

symbolsToSummarize :: [Text]
symbolsToSummarize = ["Function", "Method", "Class", "Module"]
