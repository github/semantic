{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Tags.Tagging.Precise
( Tags
, Tag(..)
, OneIndexedSpan(..)
, UTF16CodeUnitSpan(..)
, ToTags(..)
, yield
, runTagging
, calculateLineAndSpans
, countUtf16CodeUnits
, surroundingLine
, surroundingLineRange
) where

import Control.Applicative
import Control.Carrier.Reader
import Control.Carrier.Writer.Strict
import qualified Data.ByteString as B
import Data.Char (ord)
import Data.Functor.Identity
import Data.Monoid (Endo (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Proto.Semantic as P
import Source.Loc
import qualified Source.Range as Range (start)
import Source.Source as Source
import Source.Span (Pos (..), end, start)
import Tags.Tag
import Prelude hiding (span)


type Tags = Endo [Tag]

class ToTags t where
  tags :: Source -> t Loc -> [Tag]


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
  Source -> -- | ^ Source
  Loc ->    -- | ^ Location of identifier
  (Text, OneIndexedSpan, UTF16CodeUnitSpan)
calculateLineAndSpans
  src
  Loc
    { byteRange = srcRange,
      span =
        span@Span
          { start = start@Pos {column = startCol},
            end = end@Pos {column = endCol}
          }
    } = (line, toOneIndexed span, utf16Span)
  where
    -- NB: Important to limit to 180 characters after converting to text so as not to take in the middle of a multi-byte character.
    -- line = Text.strip . Text.take 180 . Source.toText $ srcLine
    line = sliceCenter180 startCol . Source.toText $ srcLine
    srcLine = surroundingLine src srcRange
    toOneIndexed (Span (Pos l1 c1) (Pos l2 c2)) = OneIndexedSpan $ Span (Pos (l1 + 1) (c1 + 1)) (Pos (l2 + 1) (c2 + 1))
    utf16Span = UTF16CodeUnitSpan $ Span start {column = utf16cpStartOffset} end {column = utf16cpEndOffset}

    utf16cpStartOffset = countUtf16CodeUnits startSlice
    utf16cpEndOffset = utf16cpStartOffset + countUtf16CodeUnits endSlice

    -- NB: Slice out of the Source ByteString, NOT Text because Loc/Range is in units of bytes.
    startSlice = Source.slice srcLine (Range 0 startCol)
    endSlice = Source.slice srcLine (Range startCol endCol)

    -- Slice out up to 180 characters around an index. Favors including the
    -- identifier and all succeeding text before including any preceeding context
    sliceCenter180 :: Int -> Text -> Text
    sliceCenter180 start txt = lhs <> rhs
      where
        (h, t) = Text.splitAt start txt
        rhs = Text.stripEnd . Text.take 180 $ t
        quota = 180 - Text.length rhs
        lhs = Text.stripStart . Text.take quota $ h

data Counter = Counter { _skip :: Int, unCounter :: Int }

countUtf16CodeUnits :: Source -> Int
countUtf16CodeUnits = unCounter . B.foldl' count (Counter 0 0) . bytes
  where
    count (Counter skip sum) !byte
      | skip > 0     = Counter (pred skip) sum
      | byte <= 0x7f = Counter 0 (1 + sum) -- takes 2 bytes (1 utf16 cu)
      | byte <= 0xbf = error "not valid utf8, byte <= 0xbf"
      | byte <= 0xdf = Counter 1 (1 + sum) -- takes 2 bytes (1 utf16 cu)
      | byte <= 0xef = Counter 2 (1 + sum)
      | byte <= 0xf7 = Counter 3 (2 + sum) -- takes 4 bytes (2 utf16 cu)
      | otherwise    = error "not valid utf8"
{-# INLINE countUtf16CodeUnits #-}

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
