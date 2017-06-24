{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Data.Source
( Source
, sourceBytes
, fromBytes
-- Measurement
, sourceLength
, nullSource
, totalRange
, totalSpan
-- En/decoding
, fromText
, toText
-- Slicing
, slice
, dropSource
-- Splitting
, sourceLines
, sourceLineRanges
, sourceLineRangesWithin
-- Conversion
, spanToRange
, spanToRangeInLineRanges
, sourceLineRangesByLineNumber
, rangeToSpan
-- Listable
, ListableByteString(..)
) where

import Data.Array
import qualified Data.ByteString as B
import Data.Char (ord)
import Data.List (span)
import Data.Range
import Data.Span
import Data.String (IsString(..))
import qualified Data.Text as T
import Prologue
import Test.LeanCheck

-- | The contents of a source file, represented as a 'ByteString'.
newtype Source = Source { sourceBytes :: B.ByteString }
  deriving (Eq, IsString, Show)

fromBytes :: ByteString -> Source
fromBytes = Source


-- Measurement

sourceLength :: Source -> Int
sourceLength = B.length . sourceBytes

nullSource :: Source -> Bool
nullSource = B.null . sourceBytes

-- | Return a 'Range' that covers the entire text.
totalRange :: Source -> Range
totalRange = Range 0 . B.length . sourceBytes

-- | Return a 'Span' that covers the entire text.
totalSpan :: Source -> Span
totalSpan source = Span (Pos 1 1) (Pos (length ranges) (succ (end lastRange - start lastRange)))
  where ranges = sourceLineRanges source
        Just lastRange = getLast (foldMap (Last . Just) ranges)


-- En/decoding

-- | Return a 'Source' from a 'ByteString'.
fromText :: T.Text -> Source
fromText = Source . encodeUtf8

-- | Return the ByteString contained in the 'Source'.
toText :: Source -> Text
toText = decodeUtf8 . sourceBytes


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


-- Splitting

-- | Split the contents of the source after newlines.
sourceLines :: Source -> [Source]
sourceLines source = (`slice` source) <$> sourceLineRanges source

-- | Compute the 'Range's of each line in a 'Source'.
sourceLineRanges :: Source -> [Range]
sourceLineRanges source = sourceLineRangesWithin (totalRange source) source

-- | Compute the 'Range's of each line in a 'Range' of a 'Source'.
sourceLineRangesWithin :: Range -> Source -> [Range]
sourceLineRangesWithin range = uncurry (zipWith Range) . ((start range:) &&& (<> [ end range ])) . fmap (+ succ (start range)) . B.elemIndices (toEnum (ord '\n')) . sourceBytes . slice range


-- Conversion

-- | Compute the byte 'Range' corresponding to a given 'Span' in a 'Source'.
spanToRange :: Source -> Span -> Range
spanToRange source = spanToRangeInLineRanges (sourceLineRangesByLineNumber source)

spanToRangeInLineRanges :: Array Int Range -> Span -> Range
spanToRangeInLineRanges lineRanges Span{..} = Range
  (start (lineRanges ! posLine spanStart) + pred (posColumn spanStart))
  (start (lineRanges ! posLine spanEnd)   + pred (posColumn spanEnd))

sourceLineRangesByLineNumber :: Source -> Array Int Range
sourceLineRangesByLineNumber source = listArray (1, length lineRanges) lineRanges
  where lineRanges = sourceLineRanges source

-- | Compute the 'Span' corresponding to a given byte 'Range' in a 'Source'.
rangeToSpan :: Source -> Range -> Span
rangeToSpan source (Range rangeStart rangeEnd) = Span startPos endPos
  where startPos = Pos (firstLine + 1)                 (rangeStart - start firstRange + 1)
        endPos =   Pos (firstLine + length lineRanges) (rangeEnd   - start lastRange  + 1)
        firstLine = length before
        (before, rest) = span ((< rangeStart) . end) (sourceLineRanges source)
        (lineRanges, _) = span ((<= rangeEnd) . start) rest
        Just firstRange = getFirst (foldMap (First . Just) lineRanges)
        Just lastRange  = getLast  (foldMap (Last  . Just) lineRanges)


-- Instances

instance Semigroup Source where
  Source a <> Source b = Source (a <> b)

instance Monoid Source where
  mempty = Source B.empty
  mappend = (<>)

instance Listable Source where
  tiers = (Source . unListableByteString) `mapT` tiers

newtype ListableByteString = ListableByteString { unListableByteString :: B.ByteString }

instance Listable ListableByteString where
  tiers = (ListableByteString . encodeUtf8 . T.pack) `mapT` strings
    where strings = foldr ((\\//) . listsOf . toTiers) []
            [ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']
            , [' '..'/'] <> [':'..'@'] <> ['['..'`'] <> ['{'..'~']
            , [chr 0x00..chr 0x1f] <> [chr 127] -- Control characters.
            , [chr 0xa0..chr 0x24f] ] -- Non-ASCII.

instance StringConv Source ByteString where
  strConv _ = sourceBytes
