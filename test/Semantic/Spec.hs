module Semantic.Spec (spec) where

import Data.Diff
import Data.Patch
import Semantic.Api hiding (Blob)
import Semantic.Git
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

  describe "git ls-tree parsing" $ do
    it "parses a git output string" $ do
      let input = "100644 tree abcdef\t/this/is/the/path"
      let expected = Right $ TreeEntry NormalMode TreeObject (OID "abcdef") "/this/is/the/path"
      parseEntry input `shouldBe` expected

    it "allows whitespace in the path" $ do
      let input = "100644 tree 12345\t/this\n/is\t/the /path\r"
      let expected = Right $ TreeEntry NormalMode TreeObject (OID "12345") "/this\n/is\t/the /path\r"
      parseEntry input `shouldBe` expected

    it "parses many outputs separated by \\NUL" $ do
      let input = "100644 tree abcdef\t/this/is/the/path\NUL120000 blob 17776\t/dev/urandom\NUL\n"
      let expected = [ TreeEntry NormalMode TreeObject (OID "abcdef") "/this/is/the/path", TreeEntry SymlinkMode BlobObject (OID "17776") "/dev/urandom"]
      parseEntries input `shouldBe` expected

  where
    methodsBlob = makeBlob "def foo\nend\n" "methods.rb" Ruby mempty
