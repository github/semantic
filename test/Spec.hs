module Main where

import qualified InterpreterSpec
import qualified OrderedMapSpec
import qualified PatchOutputSpec
import qualified SplitSpec
import qualified TermSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Interpreter" InterpreterSpec.spec
  describe "OrderedMap" OrderedMapSpec.spec
  describe "PatchOutput" PatchOutputSpec.spec
  describe "Split" SplitSpec.spec
  describe "Term" TermSpec.spec
