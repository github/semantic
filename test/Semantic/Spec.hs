{-# LANGUAGE OverloadedStrings #-}
module Semantic.Spec (spec) where

import Control.Effect.Reader
import Control.Exception (fromException)
import SpecHelpers

import Data.Blob (NoLanguageForBlob (..))
import Semantic.Api hiding (Blob)

 -- we need some lenses here, oof
setBlobLanguage :: Language -> Blob -> Blob
setBlobLanguage lang b = b { blobFile = (blobFile b) { fileLanguage = lang }}

spec :: Spec
spec = do
  describe "parseBlob" $ do
    let methodsBlob = makeBlob "def foo\nend\n" "methods.rb" Ruby mempty

    it "returns error if given an unknown language (json)" $ do
      output <- fmap runBuilder . runTaskOrDie . runReader (PerLanguageModes ALaCarte) $ parseTermBuilder TermJSONTree [ setBlobLanguage Unknown methodsBlob ]
      output `shouldBe` "{\"trees\":[{\"path\":\"methods.rb\",\"error\":\"NoLanguageForBlob \\\"methods.rb\\\"\",\"language\":\"Unknown\"}]}\n"

    it "throws if given an unknown language for sexpression output" $ do
      res <- runTaskWithOptions defaultOptions (runReader (PerLanguageModes ALaCarte) (runParseWithConfig (parseTermBuilder TermSExpression [setBlobLanguage Unknown methodsBlob])))
      case res of
        Left exc    -> fromException exc `shouldBe` Just (NoLanguageForBlob "methods.rb")
        Right _bad  -> fail "Expected parseTermBuilder to fail for an unknown language"

    it "renders with the specified renderer" $ do
      output <- fmap runBuilder . runTaskOrDie . runReader (PerLanguageModes ALaCarte) $ parseTermBuilder TermSExpression [methodsBlob]
      output `shouldBe` "(Statements\n  (Method\n    (Empty)\n    (Identifier)\n    (Statements)))\n"
