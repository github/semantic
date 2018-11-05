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
