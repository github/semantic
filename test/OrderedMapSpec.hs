module OrderedMapSpec where

import Data.Map as Map

orderedMapSpec :: IO ()
orderedMapSpec = hspec $ do
  describe "OrderedMap" $ do
    describe "difference" $ do
      it "should return those elements of a not in b" $
        Map.difference (Map.fromList [ ("a", 1), ("b", 2), ("c", 3) ]) (Map.fromList [ ("b", 2), ("c", 3), ("d", 4) ]) `shouldBe` (Map.fromList [ ("a", 1) ])
