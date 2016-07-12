module OrderedMapSpec where

import Data.Align
import qualified Data.OrderedMap as Map
import Data.Text.Arbitrary
import Data.These
import Prologue
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "difference" $ do
    it "should return those elements of a not in b" $
      Map.difference a b `shouldBe` Map.fromList [ ("a", 1) :: (Text, Integer) ]

    it "is asymmetrical" $
      Map.difference a b `shouldNotBe` Map.difference b a

  describe "union" $ do
    it "should return those elements in either a or b" $
      Map.union a b `shouldBe` Map.fromList (Map.toList a <> [ ("d", -4) ])

    it "is asymmetrical" $
      Map.union a b `shouldNotBe` Map.union b a

  describe "unions" $ do
    it "is equivalent to `union` for two maps" $
      Map.unions [ a, b ] `shouldBe` Map.union a b

    it "does not duplicate elements" $
      Map.unions [ a, b, a, b, a, b ] `shouldBe` Map.union a b

  describe "intersectionWith" $ do
    it "should return those elements in both a and b, combined with a function" $
      Map.intersectionWith (-) a b `shouldBe` Map.fromList [ ("b", 4), ("c", 6) ]

    it "is asymmetrical" $
      Map.intersectionWith (-) a b `shouldNotBe` Map.intersectionWith (-) b a

  describe "keys" $ do
    it "should return all the keys in a map" $
      Map.keys a `shouldBe` [ "a", "b", "c" ]

    it "is ordered" $
      Map.keys (Map.union b a) `shouldBe` [ "b", "c", "d", "a" ]

  describe "align" $ do
    prop "has nil as a right-identity modulo This" $
      \ a -> align a nil `shouldBe` (This <$> a :: Map.OrderedMap Text (These Text Text))

    prop "has nil as a left-identity modulo That" $
      \ a -> align nil a `shouldBe` (That <$> a :: Map.OrderedMap Text (These Text Text))

  where a = Map.fromList [ ("a", 1), ("b", 2), ("c", 3) ]
        b = Map.fromList [ ("b", -2), ("c", -3), ("d", -4) ]
