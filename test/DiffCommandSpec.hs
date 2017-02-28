module DiffCommandSpec where

import Prologue
import Data.Functor.Both
import Test.Hspec hiding (shouldBe, shouldNotBe, shouldThrow, errorCall)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck
import Arguments
import DiffCommand

spec :: Spec
spec = parallel $ do
  context "diff" $ do
    prop "all formats should produce output for file paths" $
      \format -> do
        output <- diff $ args' "" (both "test/fixtures/ruby/and-or.A.rb" "test/fixtures/ruby/and-or.B.rb") format
        output `shouldNotBe` ""

    prop "all formats should produce output for commit range" $
      \format -> do
        output <- diff $ args "test/fixtures/git/examples/all-languages.git" "dfac8fd681b0749af137aebf3203e77a06fbafc2" "2e4144eb8c44f007463ec34cb66353f0041161fe" ["methods.rb"] format
        output `shouldNotBe` ""
