module OrderedMapSpec where

import Data.Map as Map
import Test.Hspec

spec :: Spec
spec = do
  describe "difference" $ do
    it "should return those elements of a not in b" $
      Map.difference a b `shouldBe` (Map.fromList [ ("a", 1) ])

  describe "union" $ do
    it "should return those elements in either a or b" $
      Map.union a b `shouldBe` (Map.fromList $ ("a", 1) : Map.toList b)

  where a = Map.fromList [ ("a", 1), ("b", 2), ("c", 3) ]
        b = Map.fromList [ ("b", 2), ("c", 3), ("d", 4) ]
