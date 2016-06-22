module Data.Gram.Spec where

import Prologue
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "pqGrams" $
    it "exists" pending
