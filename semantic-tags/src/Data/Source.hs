{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Data.Source
( Source
, sourceBytes
, fromUTF8
-- Measurement
, sourceLength
, nullSource
, totalRange
-- En/decoding
, fromText
, toText
-- Slicing
, slice
, dropSource
) where

import           Data.Aeson (FromJSON (..), withText)
import qualified Data.ByteString as B
import           Data.Range
import           Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Generics


-- | The contents of a source file. This is represented as a UTF-8
-- 'ByteString' under the hood. Construct these with 'fromUTF8'; obviously,
-- passing 'fromUTF8' non-UTF8 bytes will cause crashes.
newtype Source = Source { sourceBytes :: B.ByteString }
  deriving (Eq, Semigroup, Monoid, IsString, Show, Generic)

fromUTF8 :: B.ByteString -> Source
fromUTF8 = Source

instance FromJSON Source where
  parseJSON = withText "Source" (pure . fromText)

-- Measurement

sourceLength :: Source -> Int
sourceLength = B.length . sourceBytes

nullSource :: Source -> Bool
nullSource = B.null . sourceBytes

-- | Return a 'Range' that covers the entire text.
totalRange :: Source -> Range
totalRange = Range 0 . B.length . sourceBytes


-- En/decoding

-- | Return a 'Source' from a 'Text'.
fromText :: T.Text -> Source
fromText = Source . T.encodeUtf8

-- | Return the Text contained in the 'Source'.
toText :: Source -> T.Text
toText = T.decodeUtf8 . sourceBytes


-- | Return a 'Source' that contains a slice of the given 'Source'.
slice :: Range -> Source -> Source
slice range = take . drop
  where drop = dropSource (start range)
        take = takeSource (rangeLength range)

dropSource :: Int -> Source -> Source
dropSource i = Source . drop . sourceBytes
  where drop = B.drop i

takeSource :: Int -> Source -> Source
takeSource i = Source . take . sourceBytes
  where take = B.take i
