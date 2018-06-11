{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Source
( Source
, sourceBytes
, fromUTF8
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
, newlineIndices
) where

import Prologue
import Data.Array
import Data.Aeson (FromJSON (..), withText)
import qualified Data.ByteString as B
import Data.Char (ord)
import Data.List (span)
import Data.Range
import Data.Span
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Proto3.Suite

-- | The contents of a source file. This is represented as a UTF-8
-- 'ByteString' under the hood. Construct these with 'fromUTF8'; obviously,
-- passing 'fromUTF8' non-UTF8 bytes will cause crashes.
newtype Source = Source { sourceBytes :: B.ByteString }
  deriving (Eq, IsString, Show, Generic, MessageField)

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

-- | Return a 'Span' that covers the entire text.
totalSpan :: Source -> Span
totalSpan source = Span (Pos 1 1) (Pos (length ranges) (succ (end lastRange - start lastRange)))
  where ranges = sourceLineRanges source
        lastRange = fromMaybe emptyRange (getLast (foldMap (Last . Just) ranges))


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


-- Splitting

-- | Split the contents of the source after newlines.
sourceLines :: Source -> [Source]
sourceLines source = (`slice` source) <$> sourceLineRanges source

-- | Compute the 'Range's of each line in a 'Source'.
sourceLineRanges :: Source -> [Range]
sourceLineRanges source = sourceLineRangesWithin (totalRange source) source

-- | Compute the 'Range's of each line in a 'Range' of a 'Source'.
sourceLineRangesWithin :: Range -> Source -> [Range]
sourceLineRangesWithin range = uncurry (zipWith Range)
                             . ((start range:) &&& (<> [ end range ]))
                             . fmap (+ succ (start range))
                             . newlineIndices
                             . sourceBytes
                             . slice range

-- | Return all indices of newlines ('\n', '\r', and '\r\n') in the 'ByteString'.
newlineIndices :: B.ByteString -> [Int]
newlineIndices = go 0
  where go n bs | B.null bs = []
                | otherwise = case (searchCR bs, searchLF bs) of
                    (Nothing, Nothing)  -> []
                    (Just i, Nothing)   -> recur n i bs
                    (Nothing, Just i)   -> recur n i bs
                    (Just crI, Just lfI)
                      | succ crI == lfI -> recur n lfI bs
                      | otherwise       -> recur n (min crI lfI) bs
        recur n i bs = let j = n + i in j : go (succ j) (B.drop (succ i) bs)
        searchLF = B.elemIndex (toEnum (ord '\n'))
        searchCR = B.elemIndex (toEnum (ord '\r'))

{-# INLINE newlineIndices #-}


-- Conversion

-- | Compute the byte 'Range' corresponding to a given 'Span' in a 'Source'.
spanToRange :: Source -> Span -> Range
spanToRange = spanToRangeInLineRanges . sourceLineRangesByLineNumber

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
        firstRange = fromMaybe emptyRange (getFirst (foldMap (First . Just) lineRanges))
        lastRange  = fromMaybe firstRange (getLast (foldMap (Last . Just) lineRanges))


-- Instances

instance Semigroup Source where
  Source a <> Source b = Source (a <> b)

instance Monoid Source where
  mempty = Source B.empty
  mappend = (<>)
