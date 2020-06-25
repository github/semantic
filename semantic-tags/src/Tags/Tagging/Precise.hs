{-# LANGUAGE NamedFieldPuns #-}
module Tags.Tagging.Precise
( Tags
, Tag(..)
, ToTags(..)
, yield
, runTagging
, firstLine
, firstLineAndSpans
, surroundingLine
, surroundingLineRange
) where

import Control.Applicative
import Control.Carrier.Reader
import Control.Carrier.Writer.Strict
import Data.Functor.Identity
import Data.Monoid (Endo (..))
import Data.Text as Text (Text, take, takeWhile, stripEnd, foldr, take)
import Prelude hiding (span)
import Source.Loc
import Source.Span (Pos(..), start, end)
import Source.Source as Source
import Tags.Tag

import qualified Data.ByteString as B
import Data.Bits ((.&.))
import Data.Char (ord)

type Tags = Endo [Tag]

class ToTags t where
  tags :: Source -> t Loc -> [Tag]


yield :: Has (Writer Tags) sig m => Tag -> m ()
yield = tell . Endo . (:) . modSpan toOneIndexed where
  modSpan f t@Tag{ tagLoc = l } = t { tagLoc = l { span = f (span l) } }
  toOneIndexed (Span (Pos l1 c1) (Pos l2 c2)) = Span (Pos (l1 + 1) (c1 + 1)) (Pos (l2 + 1) (c2 + 1))

runTagging :: Source -> ReaderC Source (WriterC Tags Identity) () -> [Tag]
runTagging source
  = ($ [])
  . appEndo
  . run
  . execWriter
  . runReader source

-- | Slices a range out of 'Source' and gives back the first line of source up to 180 characters.
firstLine :: Source -> Range -> Text
firstLine src = Text.stripEnd . Text.take 180 . Text.takeWhile (/= '\n') . Source.toText . slice src

-- A 1-indexed Span where the units are bytes.
type OneIndexedSpan = Span

-- | A 0-indxed Span where the units are utf-16 code units (2 bytes), suitable for the LSP (Language Server Protocol) specification
type UTF16CodeUnitSpan = Span

-- | Takes a Loc (where the span's column offset is measured in bytes) and
-- returns two Spans: A 1-indexed span LSP friendly span (where column offset is measure in utf16 code
-- units).
firstLineAndSpans :: Source -> Loc -> (Text, OneIndexedSpan, UTF16CodeUnitSpan)
firstLineAndSpans src Loc {byteRange, span = span@Span {start = start@Pos {column = startCol}, end = end@Pos {column = endCol}}} =
  (line, toOneIndexed span, Span start {column = utf16cpStartOffset} end {column = utf16cpEndOffset})
  where
    -- NB: Important to limit to 180 characters after converting to text so as
    -- not to take in the middle of a multi-byte character.
    line = Text.stripEnd . Text.take 180 . Source.toText $ srcLine
    srcLine = surroundingLine src byteRange
    toOneIndexed (Span (Pos l1 c1) (Pos l2 c2)) = Span (Pos (l1 + 1) (c1 + 1)) (Pos (l2 + 1) (c2 + 1))

    utf16cpStartOffset = countUtf16CodeUnits startSlice
    utf16cpEndOffset = utf16cpStartOffset + countUtf16CodeUnits endSlice

    -- NB: Slice out of the Source ByteString, NOT Text because Loc/Range is in units of bytes.
    startSlice = Source.slice srcLine (Range 0 startCol)
    endSlice = Source.slice srcLine (Range startCol endCol)

countUtf16CodeUnits :: Source -> Int
countUtf16CodeUnits = Text.foldr len 0 . Source.toText
  where
    len :: Char -> Int -> Int
    len i acc =
      let c = fromEnum i
          x = if c .&. 0xFFFF == c then 1 else 2
      in x + acc

-- | The Source of the entire surrounding line.
surroundingLine :: Source -> Range -> Source
surroundingLine src = Source.slice src . surroundingLineRange src

-- | Find the Range of the line surrounding the given Range where a line is defined by `\n`, `\r\n`, or `\r`.
surroundingLineRange :: Source -> Range -> Range
surroundingLineRange src (Range start end) = Range lineStart lineEnd
  where
    lineStart = maybe start (start -) $ B.elemIndex lfChar precedingSource <|> B.elemIndex crChar precedingSource
    precedingSource = B.reverse $ bytes (Source.slice src (Range 0 start))

    lineEnd = maybe end (start +) $ B.elemIndex crChar remainingSource <|> B.elemIndex lfChar remainingSource
    remainingSource = bytes $ Source.slice src (Range start eof)

    lfChar = toEnum (ord '\n')
    crChar = toEnum (ord '\r')
    eof = Source.length src
