module Data.Mergeable.Spec where

import Data.Functor.Identity
import Data.Mergeable
import Prologue
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "sequenceAlt" $ do
    prop "identity" $
      \ a -> sequenceAlt (fmap Just a) `shouldBe` Just (a :: [Char])
