module SemanticCmdLineSpec where

import Prologue
import Arguments
import SemanticCmdLine
import Test.Hspec hiding (shouldBe, shouldNotBe, shouldThrow, errorCall)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck

spec :: Spec
spec = parallel $ do
  -- describe "runDiff" $ do
  --   prop "produces diffs for all formats" $
  --     \ encoder -> do
  --         let mode = DiffPaths "test/fixtures/ruby/and-or.A.rb" "test/fixtures/ruby/and-or.B.rb"
  --         output <- runDiff $ DiffArguments encoder mode "" []
  --         output `shouldNotBe` ""
  describe "runParse" $ do
    it "sExpression" $ do
      let mode = ParsePaths ["test/fixtures/ruby/and-or.A.rb"]
      output <- runParse $ sExpressionParseTree mode False "" []
      output `shouldNotBe` ""
  --   prop "produces parse trees for all formats" $
  --     \ renderer -> do
  --         let mode = ParsePaths ["test/fixtures/ruby/and-or.A.rb"]
  --         output <- runParse $ ParseArguments renderer mode False "" []
  --         output `shouldNotBe` ""
