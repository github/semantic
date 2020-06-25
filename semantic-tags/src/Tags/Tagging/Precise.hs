{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Tags.Tagging.Precise
( Tags
, Tag(..)
, ToTags(..)
, yield
, runTagging
, calculateLineAndSpans
, surroundingLine
, surroundingLineRange
) where

import Control.Applicative
import Control.Carrier.Reader
import Control.Carrier.Writer.Strict
import Data.Functor.Identity
import Data.Monoid (Endo (..))
import Data.Text as Text (Text, take, strip, foldr, take)
import Prelude hiding (span)
import Source.Loc
import Source.Span (Pos(..), start, end)
import Source.Source as Source
import qualified Source.Range as Range (start)
import Tags.Tag
import qualified Proto.Semantic as P

import qualified Data.ByteString as B
import Data.Bits ((.&.))
import Data.Char (ord)

type Tags = Endo [Tag]

class ToTags t where
  tags :: Source -> t Loc -> [Tag]


-- yield :: Has (Writer Tags) sig m => Tag -> m ()
-- yield = tell . Endo . (:) . modSpan toOneIndexed where
--   modSpan f t@Tag{ tagLoc = l } = t { tagLoc = l { span = f (span l) } }
--   toOneIndexed (Span (Pos l1 c1) (Pos l2 c2)) = Span (Pos (l1 + 1) (c1 + 1)) (Pos (l2 + 1) (c2 + 1))

yield ::
  (Has (Reader Source) sig m, Has (Writer Tags) sig m) =>
  Text ->         -- |^ Text of the identifier
  P.SyntaxType -> -- |^ Type of syntax
  P.NodeType ->   -- |^ Node type: definition or reference
  Loc ->          -- |^ Location of the identifier
  Range ->        -- |^ Range of the entire expression (not used, but reserved for future used)
  m ()
yield name syntaxType nodeType loc _ = do
  src <- ask @Source
  let (line, span, lspSpan) = calculateLineAndSpans src loc
  tell . Endo . (:) $
    Tag name syntaxType nodeType span line lspSpan

runTagging :: Source -> ReaderC Source (WriterC Tags Identity) () -> [Tag]
runTagging source
  = ($ [])
  . appEndo
  . run
  . execWriter
  . runReader source

-- | Takes a Loc (where the span's column offset is measured in bytes) and
-- returns two Spans: A 1-indexed span LSP friendly span (where column offset is
-- measure in utf16 code units).
calculateLineAndSpans ::
  Source -> -- |^ Source
  Loc ->    -- |^ Location of identifier
  (Text, OneIndexedSpan, UTF16CodeUnitSpan)
calculateLineAndSpans src Loc {byteRange = srcRange, span = span@Span {start = start@Pos {column = startCol}, end = end@Pos {column = endCol}}} =
  (line, toOneIndexed span, Span start {column = utf16cpStartOffset} end {column = utf16cpEndOffset})
  where
    -- NB: Important to limit to 180 characters after converting to text so as not to take in the middle of a multi-byte character.
    line = Text.strip . Text.take 180 . Source.toText $ srcLine
    srcLine = surroundingLine src srcRange
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
surroundingLine src = Source.slice src . surroundingLineRange src . Range.start

-- | Find the Range of the line surrounding the given Position where a line is defined by `\n`, `\r\n`, or `\r`.
surroundingLineRange :: Source -> Int -> Range
surroundingLineRange src start = Range lineStart lineEnd
  where
    lineStart = maybe 0 (start -) $ B.elemIndex lfChar precedingSource <|> B.elemIndex crChar precedingSource
    precedingSource = B.reverse $ bytes (Source.slice src (Range 0 start))

    lineEnd = maybe eof (start +) $ B.elemIndex crChar remainingSource <|> B.elemIndex lfChar remainingSource
    remainingSource = bytes $ Source.slice src (Range start eof)

    lfChar = toEnum (ord '\n')
    crChar = toEnum (ord '\r')
    eof = Source.length src
