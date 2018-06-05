{-# LANGUAGE TypeApplications #-}

module Proto3.Roundtrip (spec) where

import SpecHelpers

import Data.Span
import Data.ByteString.Lazy (toStrict)
import Data.Source
import Proto3.Suite

shouldRoundtrip :: (Eq a, Show a, Message a) => a -> Expectation
shouldRoundtrip a = go a `shouldBe` Right a
  where go = fromByteString . toStrict . toLazyByteString

spec :: Spec
spec = parallel $ do
  describe "spans" $
    prop "roundtrips" $
      \sp -> shouldRoundtrip @Span sp

  describe "blobs" $ do
    it "should roundtrip given a Message instance" $ do
      let bl = Blob (fromUTF8 "puts 'hi'") "example.rb" Ruby
      shouldRoundtrip bl
