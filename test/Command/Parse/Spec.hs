module Command.Parse.Spec where

import Command.Parse
import Control.Monad
import Prelude
import Test.Hspec hiding (shouldBe, shouldNotBe, shouldThrow, errorCall)
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = parallel . context "parse" $ do
    let blobs = sourceBlobsFromPaths ["test/fixtures/ruby/and-or.A.rb"]
    it "should produce s-expression trees" $ do
      output <- sExpressionParseTree False =<< blobs
      output `shouldNotBe` ""
    it "should produce JSON trees" $ do
      output <- jsonParseTree False =<< blobs
      output `shouldNotBe` ""
    it "should produce JSON index" $ do
      output <- jsonIndexParseTree False =<< blobs
      output `shouldNotBe` ""
