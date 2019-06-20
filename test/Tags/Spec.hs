module Tags.Spec (spec) where

import Data.Text (Text)
import SpecHelpers
import Tags.Tagging


spec :: Spec
spec = do
  describe "go" $ do
    it "produces tags for functions with docs" $ do
      (blob, tree) <- parseTestFile goParser "test/fixtures/go/tags/simple_functions.go"
      runTagging blob symbolsToSummarize tree `shouldBe`
        [ Tag "TestFromBits" "Function" (Span (Pos 6 1) (Pos 8 2)) ["Statements"] (Just "func TestFromBits(t *testing.T) {") (Just "// TestFromBits ...")
        , Tag "Hi" "Function" (Span (Pos 10 1) (Pos 11 2)) ["Statements"] (Just "func Hi()") Nothing ]

    it "produces tags for methods" $ do
      (blob, tree) <- parseTestFile goParser "test/fixtures/go/tags/method.go"
      runTagging blob symbolsToSummarize tree `shouldBe`
        [ Tag "CheckAuth" "Method" (Span (Pos 3 1) (Pos 3 100)) ["Statements"] (Just "func (c *apiClient) CheckAuth(req *http.Request, user, repo string) (*authenticatedActor, error)") Nothing]

    it "produces tags for calls" $ do
      (blob, tree) <- parseTestFile goParser "test/fixtures/go/tags/simple_functions.go"
      runTagging blob ["Call"] tree `shouldBe`
        [ Tag "Hi" "Call" (Span (Pos 7 2) (Pos 7 6)) ["Function", "Context", "Statements"] (Just "Hi()") Nothing]

  describe "javascript and typescript" $ do
    it "produces tags for functions with docs" $ do
      (blob, tree) <- parseTestFile typescriptParser "test/fixtures/javascript/tags/simple_function_with_docs.js"
      runTagging blob symbolsToSummarize tree `shouldBe`
        [ Tag "myFunction" "Function" (Span (Pos 2 1) (Pos 4 2)) ["Statements"] (Just "function myFunction()") (Just "// This is myFunction") ]

    it "produces tags for classes" $ do
      (blob, tree) <- parseTestFile typescriptParser "test/fixtures/typescript/tags/class.ts"
      runTagging blob symbolsToSummarize tree `shouldBe`
        [ Tag "FooBar" "Class" (Span (Pos 1 1) (Pos 1 16)) ["Statements"] (Just "class FooBar") Nothing ]

    it "produces tags for modules" $ do
      (blob, tree) <- parseTestFile typescriptParser "test/fixtures/typescript/tags/module.ts"
      runTagging blob symbolsToSummarize tree `shouldBe`
        [ Tag "APromise" "Module" (Span (Pos 1 1) (Pos 1 20)) ["Statements"] (Just "module APromise { }") Nothing ]

  describe "python" $ do
    it "produces tags for functions" $ do
      (blob, tree) <- parseTestFile pythonParser "test/fixtures/python/tags/simple_functions.py"
      runTagging blob symbolsToSummarize tree `shouldBe`
        [ Tag "Foo" "Function" (Span (Pos 1 1) (Pos 5 17)) ["Statements"] (Just "def Foo(x):") Nothing
        , Tag "Bar" "Function" (Span (Pos 7 1) (Pos 11 13)) ["Statements"] (Just "def Bar():") Nothing
        , Tag "local" "Function" (Span (Pos 8 5) (Pos 9 17)) ["Statements", "Function", "Statements"] (Just "def local():") Nothing
        ]

    it "produces tags for functions with docs" $ do
      (blob, tree) <- parseTestFile pythonParser "test/fixtures/python/tags/simple_function_with_docs.py"
      runTagging blob symbolsToSummarize tree `shouldBe`
        [ Tag "Foo" "Function" (Span (Pos 1 1) (Pos 3 13)) ["Statements"] (Just "def Foo(x):") (Just "\"\"\"This is the foo function\"\"\"") ]

    it "produces tags for classes" $ do
      (blob, tree) <- parseTestFile pythonParser "test/fixtures/python/tags/class.py"
      runTagging blob symbolsToSummarize tree `shouldBe`
        [ Tag "Foo" "Class" (Span (Pos 1 1) (Pos 5 17)) ["Statements"] (Just "class Foo:") (Just "\"\"\"The Foo class\"\"\"")
        , Tag "f" "Function" (Span (Pos 3 5) (Pos 5 17)) ["Statements", "Class", "Statements"] (Just "def f(self):") (Just "\"\"\"The f method\"\"\"")
        ]

    it "produces tags for multi-line functions" $ do
      (blob, tree) <- parseTestFile pythonParser "test/fixtures/python/tags/multiline.py"
      runTagging blob symbolsToSummarize tree `shouldBe`
        [ Tag "Foo" "Function" (Span (Pos 1 1) (Pos 3 13)) ["Statements"] (Just "def Foo(x,") Nothing ]

  describe "ruby" $ do
    it "produces tags for methods" $ do
      (blob, tree) <- parseTestFile rubyParser "test/fixtures/ruby/tags/simple_method.rb"
      runTagging blob symbolsToSummarize tree `shouldBe`
        [ Tag "foo" "Method" (Span (Pos 1 1) (Pos 4 4)) ["Statements"] (Just "def foo") Nothing ]

    it "produces tags for sends" $ do
      (blob, tree) <- parseTestFile rubyParser "test/fixtures/ruby/tags/simple_method.rb"
      runTagging blob ["Send"] tree `shouldBe`
        [ Tag "puts" "Send" (Span (Pos 2 3) (Pos 2 12)) ["Statements", "Method", "Statements"] (Just "puts \"hi\"") Nothing
        , Tag "bar" "Send" (Span (Pos 3 3) (Pos 3 8)) ["Statements", "Method", "Statements"] (Just "a.bar") Nothing
        , Tag "a" "Send" (Span (Pos 3 3) (Pos 3 4)) ["Send", "Statements", "Method", "Statements"] (Just "a") Nothing
        ]

    it "produces tags for methods with docs" $ do
      (blob, tree) <- parseTestFile rubyParser "test/fixtures/ruby/tags/simple_method_with_docs.rb"
      runTagging blob symbolsToSummarize tree `shouldBe`
        [ Tag "foo" "Method" (Span (Pos 2 1) (Pos 3 4)) ["Statements"] (Just "def foo") (Just "# Public: foo") ]

    it "produces tags for methods and classes with docs" $ do
      (blob, tree) <- parseTestFile rubyParser "test/fixtures/ruby/tags/class_module.rb"
      runTagging blob symbolsToSummarize tree `shouldBe`
        [ Tag "Foo" "Module" (Span (Pos 2 1 ) (Pos 12 4)) ["Statements"] (Just "module Foo") (Just "# Public: Foo")
        , Tag "Bar" "Class"  (Span (Pos 5 3 ) (Pos 11 6)) ["Module", "Context", "Statements"] (Just "class Bar") (Just "# Public: Bar")
        , Tag "baz" "Method" (Span (Pos 8 5 ) (Pos 10 8)) ["Class", "Context", "Module", "Context", "Statements"] (Just "def baz(a)") (Just "# Public: baz")
        , Tag "C" "Class" (Span (Pos 14 1) (Pos 20 4)) ["Statements"] (Just "class A::B::C") Nothing
        , Tag "foo" "Method" (Span (Pos 15 3) (Pos 17 6)) ["Statements", "Class", "Statements"] (Just "def foo") Nothing
        , Tag "foo" "Method" (Span (Pos 18 3) (Pos 19 6)) ["Statements", "Class", "Statements"] (Just "def self.foo") Nothing
        ]

symbolsToSummarize :: [Text]
symbolsToSummarize = ["Function", "Method", "Class", "Module"]
