module Tags.Spec (spec) where

import Tags.Tagging
import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "ruby" $ do
    it "produces tags for methods" $ do
      (blob, tree) <- parseTestFile rubyParser "test/fixtures/ruby/tags/simple_method.rb"
      runTagging blob tree `shouldBe` Right
        [ Tag "foo" "Method" (Span (Pos 1 1) (Pos 2 4)) ["Statements"] (Just "def foo") Nothing ]

    it "produces tags for methods with docs" $ do
      (blob, tree) <- parseTestFile rubyParser "test/fixtures/ruby/tags/simple_method_with_docs.rb"
      runTagging blob tree `shouldBe` Right
        [ Tag "foo" "Method" (Span (Pos 2 1) (Pos 3 4)) ["Statements"] (Just "def foo") (Just "# Public: foo") ]

    it "produces tags for methods and classes with docs" $ do
      (blob, tree) <- parseTestFile rubyParser "test/fixtures/ruby/tags/class_module.rb"
      runTagging blob tree `shouldBe` Right
        [ Tag "Foo" "Module" (Span (Pos 2 1 ) (Pos 12 4)) ["Statements"] (Just "module Foo") (Just "# Public: Foo")
        , Tag "Bar" "Class"  (Span (Pos 5 3 ) (Pos 11 6)) ["Module", "Context", "Statements"] (Just "class Bar") (Just "# Public: Bar")
        , Tag "baz" "Method" (Span (Pos 8 5 ) (Pos 10 8)) ["Class", "Context", "Module", "Context", "Statements"] (Just "def baz(a)") (Just "# Public: baz")
        ]

  describe "python" $ do
    it "produces tags for functions" $ do
      (blob, tree) <- parseTestFile pythonParser "test/fixtures/python/tags/simple_functions.py"
      runTagging blob tree `shouldBe` Right
        [ Tag "Foo" "Function" (Span (Pos 1 1) (Pos 5 17)) ["Statements"] (Just "def Foo(x):") Nothing
        , Tag "Bar" "Function" (Span (Pos 7 1) (Pos 11 13)) ["Statements"] (Just "def Bar():") Nothing
        , Tag "local" "Function" (Span (Pos 8 5) (Pos 9 17)) ["Statements", "Function", "Statements"] (Just "def local():") Nothing
        ]

    it "produces tags for functions with docs" $ do
      (blob, tree) <- parseTestFile pythonParser "test/fixtures/python/tags/simple_function_with_docs.py"
      runTagging blob tree `shouldBe` Right
        [ Tag "Foo" "Function" (Span (Pos 1 1) (Pos 3 13)) ["Statements"] (Just "def Foo(x):") (Just "\"\"\"This is the foo function\"\"\"") ]

    it "produces tags for classes" $ do
      (blob, tree) <- parseTestFile pythonParser "test/fixtures/python/tags/class.py"
      runTagging blob tree `shouldBe` Right
        [ Tag "Foo" "Class" (Span (Pos 1 1) (Pos 5 17)) ["Statements"] (Just "class Foo:") (Just "\"\"\"The Foo class\"\"\"")
        , Tag "f" "Function" (Span (Pos 3 5) (Pos 5 17)) ["Statements", "Class", "Statements"] (Just "def f(self):") (Just "\"\"\"The f method\"\"\"")
        ]
