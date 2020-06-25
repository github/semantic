{-# LANGUAGE NamedFieldPuns #-}
module Tags.Tagging.Precise
( Tags
, Tag(..)
, ToTags(..)
, yield
, runTagging
, firstLine
, utf16CodeUnitsSpan
) where

import Control.Applicative
import Control.Carrier.Reader
import Control.Carrier.Writer.Strict
import Data.Functor.Identity
import Data.Monoid (Endo (..))
import Data.Text as Text (Text, take, takeWhile, stripEnd, foldr, drop, take)
import Prelude hiding (span)
import Source.Loc
import Source.Span (Pos(..), start, end)
import qualified Source.Range as Range
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

type OneIndexedSpan = Span
type LspStyleSpan = Span

-- | Takes a Loc (where the span's column offset is measured in utf8 bytes) and
-- returns an LSP friendly span (where column offset is measure in utf16 code units).
utf16CodeUnitsSpan :: Source -> Loc -> (Text, OneIndexedSpan, LspStyleSpan)
utf16CodeUnitsSpan src Loc{ byteRange, span = span@Span{ start = start@Pos{column = startCol}, end = end@Pos{column = endCol}}}
  = (line, toOneIndexed span, Span start {column = utf16cpStartOffset} end {column = utf16cpEndOffset})
  where
    line = Text.stripEnd . Text.take 180 $ srcLine
    srcLine = Source.toText $ Source.slice src srcLineRange
    srcLineRange = lineRange src byteRange
    toOneIndexed (Span (Pos l1 c1) (Pos l2 c2)) = Span (Pos (l1 + 1) (c1 + 1)) (Pos (l2 + 1) (c2 + 1))

    utf16cpStartOffset = countUtf16CodeUnits startSlice
    utf16cpEndOffset = utf16cpStartOffset + countUtf16CodeUnits endSlice

    startSlice = sliceText srcLine (Range 0 startCol)
    endSlice = sliceText srcLine (Range startCol endCol)

    countUtf16CodeUnits :: Text -> Int
    countUtf16CodeUnits = Text.foldr len 0
      where
        len :: Char -> Int -> Int
        len i acc =
          let c = fromEnum i
              x = if c .&. 0xFFFF == c then 1 else 2
          in x + acc

    sliceText :: Text -> Range -> Text
    sliceText source range = taking $ dropping source where
      dropping = Text.drop (Range.start range)
      taking   = Text.take (Range.rangeLength range)

lineRange :: Source -> Range -> Range
lineRange src (Range start end) = Range lineStart lineEnd
  where
    lineStart = maybe start (start -) $ B.elemIndex lfChar precedingSource <|> B.elemIndex crChar precedingSource
    precedingSource = B.reverse $ bytes (Source.slice src (Range 0 start))

    lineEnd = maybe end (start +) $ B.elemIndex crChar remainingSource <|> B.elemIndex lfChar remainingSource
    remainingSource = bytes $ Source.slice src (Range start eof)

    lfChar = toEnum (ord '\n')
    crChar = toEnum (ord '\r')
    eof = Source.length src
