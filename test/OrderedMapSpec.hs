module OrderedMapSpec where

import OrderedMap as Map
import Test.Hspec

spec :: Spec
spec = do
  describe "difference" $ do
    it "should return those elements of a not in b" $
      Map.difference a b `shouldBe` (Map.fromList [ ("a", 1) ])

    it "is asymmetrical" $ do
      Map.difference a b `shouldNotBe` Map.difference b a

  describe "union" $ do
    it "should return those elements in either a or b" $
      Map.union a b `shouldBe` (Map.fromList $ ("a", 1) : Map.toList b)

  describe "intersectionWith" $ do
    it "should return those elements in both a and b, combined with a function" $
      Map.intersectionWith (+) a b `shouldBe` (Map.fromList [ ("b", 4), ("c", 6) ])

  describe "keys" $ do
    it "should return all the keys in a map" $
      Map.keys a `shouldBe` [ "a", "b", "c" ]

  where a = Map.fromList [ ("a", 1), ("b", 2), ("c", 3) ]
        b = Map.fromList [ ("b", 2), ("c", 3), ("d", 4) ]
