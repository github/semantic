module Main where

import Prologue
import qualified AlignmentSpec
import qualified Command.Spec
import qualified Command.Diff.Spec
import qualified Command.Parse.Spec
import qualified Data.Mergeable.Spec
import qualified Data.RandomWalkSimilarity.Spec
import qualified Data.Syntax.Assignment.Spec
import qualified DiffSpec
import qualified SummarySpec
import qualified GitmonClientSpec
import qualified InterpreterSpec
import qualified PatchOutputSpec
import qualified RangeSpec
import qualified SES.Myers.Spec
import qualified SourceSpec
import qualified TermSpec
import qualified TOCSpec
import qualified IntegrationSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  parallel $ do
    describe "Alignment" AlignmentSpec.spec
    describe "Command" Command.Spec.spec
    describe "Command.Diff" Command.Diff.Spec.spec
    describe "Command.Parse" Command.Parse.Spec.spec
    describe "Data.Mergeable" Data.Mergeable.Spec.spec
    describe "Data.RandomWalkSimilarity" Data.RandomWalkSimilarity.Spec.spec
    describe "Data.Syntax.Assignment" Data.Syntax.Assignment.Spec.spec
    describe "Diff" DiffSpec.spec
    describe "Summary" SummarySpec.spec
    describe "Interpreter" InterpreterSpec.spec
    describe "PatchOutput" PatchOutputSpec.spec
    describe "Range" RangeSpec.spec
    describe "SES.Myers" SES.Myers.Spec.spec
    describe "Source" SourceSpec.spec
    describe "Term" TermSpec.spec
    describe "TOC" TOCSpec.spec
    describe "Integration" IntegrationSpec.spec


  describe "GitmonClient" GitmonClientSpec.spec
