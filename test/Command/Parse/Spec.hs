module Command.Parse.Spec where

import Command.Parse
import Prelude
import Test.Hspec hiding (shouldBe, shouldNotBe, shouldThrow, errorCall)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck
import Arguments
import Renderer

spec :: Spec
spec = parallel $ do
  context "parse" $ do
    prop "all valid formats should produce output" $
      \format ->
        case format of
          SExpressionTree -> do
            output <- parseSExpression $ parseArgs ["test/fixtures/ruby/and-or.A.rb"] format
            output `shouldNotBe` ""
          JSONIndex -> do
            output <- parseIndex $ parseArgs ["test/fixtures/ruby/and-or.A.rb"] format
            output `shouldNotBe` ""
          JSONTree -> do
            output <- parseTree $ parseArgs ["test/fixtures/ruby/and-or.A.rb"] format
            output `shouldNotBe` ""
