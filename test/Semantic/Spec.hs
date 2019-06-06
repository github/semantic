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

  describe "gitParsing" $ do
    it "parses a git output string" $ do
      let input = "100644 tree ThisIsTheOid\t/this/is/the/path"
      let expected = TreeEntry NormalMode TreeObject (OID "ThisIsTheOid") "/this/is/the/path"
      parseEntry input `shouldBe` expected

    it "parses nonsense into a default value" $ do
      let input = "iel jgh\nf2 8i4p\r8f2y4fpoxin u3y2 unz"
      let expected = TreeEntry OtherMode OtherObjectType (OID mempty) mempty
      parseEntry input `shouldBe` expected

    it "parses many outputs separated by \\NUL" $ do
      let input = "100644 tree ThisIsTheOid\t/this/is/the/path\NULiel jgh\nf2 8i4p\r8f2y4fpoxin u3y2 unz\NUL120000 blob 17776\t/dev/urandom"
      let expected = [ TreeEntry NormalMode TreeObject (OID "ThisIsTheOid") "/this/is/the/path", TreeEntry OtherMode OtherObjectType (OID mempty) mempty, TreeEntry SymlinkMode BlobObject (OID "17776") "/dev/urandom"]
      parseEntries input `shouldBe` expected

  where
    methodsBlob = makeBlob "def foo\nend\n" "methods.rb" Ruby mempty
