{-# LANGUAGE OverloadedStrings #-}
module Semantic.Spec (spec) where

import           Analysis.File
import           Control.Exception (fromException)
import qualified Data.Blob as Blob
import           SpecHelpers

import Semantic.Api hiding (Blob)

 -- we need some lenses here, oof
setBlobLanguage :: Language -> Blob -> Blob
setBlobLanguage lang b = b { blobFile = (blobFile b) { fileBody = lang }}

spec :: Spec
spec = do
  describe "parseBlob" $ do
    let methodsBlob = Blob.fromSource "methods.rb" Ruby "def foo\nend\n"

    it "throws if given an unknown language for sexpression output" $ do
      res <- runTaskWithOptions defaultOptions (runParseWithConfig (parseTermBuilder TermSExpression [setBlobLanguage Unknown methodsBlob]))
      case res of
        Left exc   -> fromException exc `shouldBe` Just (NoLanguageForBlob "methods.rb")
        Right _bad -> fail "Expected parseTermBuilder to fail for an unknown language"

    it "renders with the specified renderer" $ do
      output <- fmap runBuilder . runTaskOrDie  $ parseTermBuilder TermSExpression [methodsBlob]
      output `shouldBe` "(Program \n  (Statement \n    (Arg \n      (Primary \n        (Method \n          (MethodName \n            (Identifier \"foo\")))))))\n"
