module ParseCommandSpec where

import Data.Functor.Listable
import Prelude
import Test.Hspec hiding (shouldBe, shouldNotBe, shouldThrow, errorCall)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck
import Test.LeanCheck
import Arguments
import ParseCommand
import Renderer

spec :: Spec
spec = parallel $
  context "parse" $
    prop "all valid formats should produce output" . forAll (isParseFormat `filterT` tiers) $
      \format ->
        case format of
          SExpression -> do
            output <- parseSExpression $ parseArgs ["test/fixtures/ruby/and-or.A.rb"] format
            output `shouldNotBe` ""
          Index -> do
            output <- parseIndex $ parseArgs ["test/fixtures/ruby/and-or.A.rb"] format
            output `shouldNotBe` ""
          _ -> do
            output <- parseTree $ parseArgs ["test/fixtures/ruby/and-or.A.rb"] format
            output `shouldNotBe` ""

isParseFormat :: Format -> Bool
isParseFormat a | Index <- a = True
                | ParseTree <- a = True
                | JSON <- a = True
                | SExpression <- a = True
                | otherwise = False
