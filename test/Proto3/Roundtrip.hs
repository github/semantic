{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Proto3.Roundtrip (spec) where

import SpecHelpers

import Data.Span
import qualified Data.ByteString.Lazy as L
import Data.Source
import Proto3.Suite
import qualified Proto3.Wire.Encode as E
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import Data.Term (Term)
import Data.Sum
import Language.JSON.Assignment (Syntax)
import qualified Language.Ruby.Assignment as Ruby
import Data.Functor.Classes

shouldRoundtrip :: (Eq a, Show a, Message a) => a -> Expectation
shouldRoundtrip a = go a `shouldBe` Right a
  where go = fromByteString . L.toStrict . toLazyByteString

shouldRoundtrip' :: forall f a. (Show (f a), Eq (f a), Show1 f, Eq1 f, Eq a, Show a, Message1 f, Message a) => f a -> Expectation
shouldRoundtrip' a = go a `shouldBe` Right a
  where go = fromByteString1 . L.toStrict . toLazyByteString1

spec :: Spec
spec = parallel $ do
  describe "spans" $
    prop "roundtrips" $
      \sp -> shouldRoundtrip @Span sp

  describe "nulls" $
    prop "roundtrips" $
      \sp -> shouldRoundtrip' @Literal.Null @(Term (Sum Syntax) ()) (unListableF sp)

  describe "text elements" $
    prop "roundtrips" $
      \sp -> shouldRoundtrip' @Literal.TextElement @(Term (Sum Syntax) ()) (unListableF sp)

  describe "floats" $
    prop "roundtrips" $
      \sp -> shouldRoundtrip' @Literal.Float @(Term (Sum Syntax) ()) (unListableF sp)

  describe "booleans" $
    prop "roundtrips" $
      \sp -> shouldRoundtrip' @Literal.Boolean @(Term (Sum Syntax) ()) (unListableF sp)

  describe "terms of syntax" $
    prop "roundtrips" $
      \sp -> shouldRoundtrip @(Term (Sum Syntax) ()) (unListableF sp)

  describe "arrays" $
    prop "roundtrips" $
      \sp -> shouldRoundtrip' @Literal.Array @(Term (Sum Syntax) ()) (unListableF sp)

  describe "key values" $
    prop "roundtrips" $
      \sp -> shouldRoundtrip' @Literal.KeyValue @(Term (Sum Syntax) ()) (unListableF sp)

  describe "statements" $
    prop "roundtrips" $
      \sp -> shouldRoundtrip' @Statement.Statements @(Term (Sum Syntax) ()) (unListableF sp)

  describe "blobs" $ do
    it "should roundtrip given a Message instance" $ do
      let bl = Blob (fromUTF8 "puts 'hi'") "example.rb" Ruby
      shouldRoundtrip bl

  describe "languages" $ do
    -- If this test broke, it means you've probably added another 'Language'.
    -- Add it to the list of languages below and everything should be good,
    -- as long as you added it in a way that doesn't break prior Enum encodings.
    it "should match up with Enum declarations" $ do
      let go :: (Primitive f, MessageField f) => [f] -> [L.ByteString]
          go x = E.toLazyByteString . encodePrimitive (fieldNumber 0) <$> x
      let ints = [0..fromEnum (maxBound @Language)]
      let langs = [Unknown, Go, Haskell, Java, JavaScript, JSON,
                   JSX, Markdown, Python, Ruby, TypeScript, PHP]
      go ints `shouldBe` go langs
