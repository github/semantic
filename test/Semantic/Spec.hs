module Semantic.Spec (spec) where

import Data.Diff
import Data.Patch
import Semantic.Api hiding (Blob)
import System.Exit

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "parseBlob" $ do
    it "returns error if given an unknown language (json)" $ do
      output <- fmap runBuilder . runTaskOrDie $ parseTermBuilder TermJSONTree [ methodsBlob { blobLanguage = Unknown } ]
      output `shouldBe` "{\"trees\":[{\"path\":\"methods.rb\",\"error\":\"NoLanguageForBlob \\\"methods.rb\\\"\",\"language\":\"Unknown\"}]}\n"

    it "throws if given an unknown language for sexpression output" $ do
      runTaskOrDie (parseTermBuilder TermSExpression [methodsBlob { blobLanguage = Unknown }]) `shouldThrow` (== ExitFailure 1)

    it "renders with the specified renderer" $ do
      output <- fmap runBuilder . runTaskOrDie $ parseTermBuilder TermSExpression [methodsBlob]
      output `shouldBe` "(Statements\n  (Method\n    (Empty)\n    (Identifier)\n    (Statements)))\n"
  where
    methodsBlob = Blob "def foo\nend\n" "methods.rb" Ruby
