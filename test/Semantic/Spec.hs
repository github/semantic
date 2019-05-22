module Semantic.Spec (spec) where

import Data.Diff
import Data.Patch
import Semantic.Api hiding (Blob)
import System.Exit

import SpecHelpers

 -- we need some lenses here, oof
setBlobLanguage :: Language -> Blob -> Blob
setBlobLanguage lang b = b { blobFile = (blobFile b) { fileLanguage = lang }}

spec :: Spec
spec = parallel $ do
  describe "parseBlob" $ do
    it "returns error if given an unknown language (json)" $ do
      output <- fmap runBuilder . runTaskOrDie $ parseTermBuilder TermJSONTree [ setBlobLanguage Unknown methodsBlob ]
      output `shouldBe` "{\"trees\":[{\"path\":\"methods.rb\",\"error\":\"NoLanguageForBlob \\\"methods.rb\\\"\",\"language\":\"Unknown\"}]}\n"

    it "throws if given an unknown language for sexpression output" $ do
      runTaskOrDie (parseTermBuilder TermSExpression [setBlobLanguage Unknown methodsBlob]) `shouldThrow` (== ExitFailure 1)

    it "renders with the specified renderer" $ do
      output <- fmap runBuilder . runTaskOrDie $ parseTermBuilder TermSExpression [methodsBlob]
      output `shouldBe` "(Statements\n  (Method\n    (Empty)\n    (Identifier)\n    (Statements)))\n"
  where
    methodsBlob = legacyMakeBlob "def foo\nend\n" "methods.rb" Ruby mempty
