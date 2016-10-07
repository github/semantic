module Main where

import Prologue
import qualified SemanticGitDiffSpec
import Test.Hspec

main :: IO ()
main = hspec $ parallel $ do
  describe "DiffSummaries" SemanticGitDiffSpec.spec
