module Diff.Spec where

import Diff
import Prologue
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "mergeMaybe" $ do
    it "is symmetrical" $ pending
