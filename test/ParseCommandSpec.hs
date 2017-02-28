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
spec = parallel $ do
  context "parse" $ do
    prop "all valid formats should produce output" . forAll (isParseFormat `filterT` tiers) $
      \format -> do
        output <- parse $ parseArgs ["test/fixtures/ruby/and-or.A.rb"] format
        output `shouldNotBe` ""

isParseFormat :: Format -> Bool
isParseFormat a | JSON <- a = True
                | SExpression <- a = True
                | otherwise = False
