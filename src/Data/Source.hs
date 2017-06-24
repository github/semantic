{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Data.Source
( Source(..)
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
, drop
-- Splitting
, sourceLines
, sourceLineRanges
, sourceLineRangesWithin
-- Conversion
, spanToRange
, spanToRangeInLineRanges
, rangeToSpan
-- Listable
, ListableByteString(..)
) where

import qualified Data.ByteString as B
import Data.List (span)
import Data.Range
import Data.Span
import Data.String (IsString(..))
import qualified Data.Text as T
import Prologue hiding (break, drop, take)
import qualified Prologue
import Test.LeanCheck

-- | The contents of a source file, represented as a ByteString.
newtype Source = Source { sourceBytes :: B.ByteString }
  deriving (Eq, IsString, Show)


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
  where drop = Data.Source.drop (start range)
        take = Data.Source.take (rangeLength range)

drop :: Int -> Source -> Source
drop i = Source . drop . sourceBytes
  where drop = B.drop i

take :: Int -> Source -> Source
take i = Source . take . sourceBytes
  where take = B.take i


-- Splitting

-- | Split the source into the longest prefix of elements that do not satisfy the predicate and the rest without copying.
break :: (Word8 -> Bool) -> Source -> (Source, Source)
break predicate (Source text) = let (start, remainder) = B.break predicate text in (Source start, Source remainder)


-- | Split the contents of the source after newlines.
sourceLines :: Source -> [Source]
sourceLines = fmap Source . sourceLines' . sourceBytes
  where sourceLines' text
          | B.null text = [ text ]
          | otherwise = case B.break (== toEnum (fromEnum '\n')) text of
            (l, lines') -> case B.uncons lines' of
              Nothing -> [ l ]
              Just (_, lines') -> (l <> B.singleton (toEnum (fromEnum '\n'))) : sourceLines' lines'

-- | Compute the 'Range's of each line in a 'Source'.
sourceLineRanges :: Source -> [Range]
sourceLineRanges = Prologue.drop 1 . scanl toRange (Range 0 0) . sourceLines
  where toRange previous source = Range (end previous) $ end previous + sourceLength source

-- | Compute the 'Range's of each line in a 'Range' of a 'Source'.
sourceLineRangesWithin :: Range -> Source -> [Range]
sourceLineRangesWithin range = Prologue.drop 1 . scanl toRange (Range (start range) (start range)) . sourceLines . slice range
  where toRange previous source = Range (end previous) $ end previous + sourceLength source


-- Conversion

-- | Compute the byte 'Range' corresponding to a given 'Span' in a 'Source'.
spanToRange :: Source -> Span -> Range
spanToRange source = spanToRangeInLineRanges (sourceLineRanges source)

spanToRangeInLineRanges :: [Range] -> Span -> Range
spanToRangeInLineRanges lineRanges Span{..} = Range start end
  where start = pred (sumLengths leadingRanges + posColumn spanStart)
        end = start + sumLengths (Prologue.take (posLine spanEnd - posLine spanStart) remainingRanges) + (posColumn spanEnd - posColumn spanStart)
        (leadingRanges, remainingRanges) = splitAt (pred (posLine spanStart)) lineRanges
        sumLengths = sum . fmap rangeLength

-- | Compute the 'Span' corresponding to a given byte 'Range' in a 'Source'.
rangeToSpan :: Source -> Range -> Span
rangeToSpan source (Range rangeStart rangeEnd) = Span startPos endPos
  where startPos = Pos (firstLine + 1)                          (rangeStart - start firstRange + 1)
        endPos =   Pos (firstLine + length lineRanges) (rangeEnd   - start lastRange  + 1)
        firstLine = length before
        (before, rest) = span ((< rangeStart) . end) (sourceLineRanges source)
        (lineRanges, _) = span ((<= rangeEnd) . start) rest
        Just firstRange = getFirst (foldMap (First . Just) lineRanges)
        Just lastRange = getLast (foldMap (Last . Just) lineRanges)


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
