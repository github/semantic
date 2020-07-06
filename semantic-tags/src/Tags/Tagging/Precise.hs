{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Tags.Tagging.Precise
( Tags
, Tag(..)
, OneIndexedSpan(..)
, UTF16CodeUnitSpan(..)
, ToTags(..)
, LineIndices(..)
, yield
, runTagging
, calculateLineAndSpans
, countUtf16CodeUnits
, surroundingLine
, surroundingLineRange
) where

import Control.Carrier.Reader
import Control.Carrier.Writer.Strict
import Control.Carrier.State.Strict
import qualified Data.ByteString as B
import Data.Char (ord)
import Data.Functor.Identity
import Data.Monoid (Endo (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Proto.Semantic as P
import Source.Loc
import Control.DeepSeq
import Source.Source as Source
import Source.Span (Pos (..), end, start)
import Tags.Tag
import Prelude hiding (span)
import Data.Map as Map
import Data.IntMap as IntMap


type Tags = Endo [Tag]

class ToTags t where
  tags :: Source -> t Loc -> [Tag]


yield ::
  (Has (State LineIndices) sig m, Has (Reader Source) sig m, Has (Writer Tags) sig m) =>
  Text ->         -- |^ Text of the identifier
  P.SyntaxType -> -- |^ Type of syntax
  P.NodeType ->   -- |^ Node type: definition or reference
  Loc ->          -- |^ Location of the identifier
  Range ->        -- |^ Range of the entire expression (not used, but reserved for future used)
  m ()
yield name syntaxType nodeType loc _ = do
  src <- ask @Source
  xs <- get @LineIndices
  let (line, span, lspSpan, map) = calculateLineAndSpans src xs loc
  put map
  tell . Endo . (:) $
    Tag name syntaxType nodeType (byteRange loc) span line lspSpan

runTagging :: Source -> ReaderC Source (StateC LineIndices (WriterC Tags Identity)) () -> [Tag]
runTagging source
  = ($ [])
  . appEndo
  . run
  . execWriter
  . evalState (LineIndices mempty)
  . runReader source

type UTF16CUCount = Int

-- | LineCache is a cache of a line of source code and a map of byte offsets to utf16 code unit count.
type LineCache = (Source, IntMap.IntMap UTF16CUCount)

-- | LineIndices is a cache of row to LineCache
newtype LineIndices = LineIndices { unLineIndices :: Map.Map Int LineCache }
  deriving (Eq, Show, NFData)

-- | Takes a Loc (where the span's column offset is measured in bytes) and
-- returns two Spans: A 1-indexed span LSP friendly span (where column offset is
-- measure in utf16 code units).
calculateLineAndSpans ::
  Source -> -- | ^ Source
  LineIndices ->
  Loc ->    -- | ^ Location of identifier
  (Text, OneIndexedSpan, UTF16CodeUnitSpan, LineIndices)
calculateLineAndSpans
  src
  lineIndexes
  loc@Loc
    { span =
        span@Span
          { start = start@(Pos startRow startCol),
            end = end@(Pos _ endCol)
          }
    } = (line, toOneIndexed span, utf16Span, map')
  where
    -- NB: Important to limit to 180 characters after converting to text so as not to take in the middle of a multi-byte character.
    -- line = Text.strip . Text.take 180 . Source.toText $ srcLine
    line = sliceCenter180 startCol . Source.toText $ srcLine
    (srcLine, lineCache, map) = surroundingLine src lineIndexes loc
    toOneIndexed (Span (Pos l1 c1) (Pos l2 c2)) = OneIndexedSpan $ Span (Pos (l1 + 1) (c1 + 1)) (Pos (l2 + 1) (c2 + 1))
    utf16Span = UTF16CodeUnitSpan $ Span start {column = utf16cpStartOffset} end {column = utf16cpEndOffset}

    (utf16cpStartOffset, map') = countOffsetCached startSlice lineCache startCol map
    utf16cpEndOffset = utf16cpStartOffset + countUtf16CodeUnits endSlice

    countOffsetCached :: Source -> LineCache -> Int -> LineIndices -> (Int, LineIndices)
    countOffsetCached slice (s, countMap) colKey (LineIndices map)
      = let c = case IntMap.lookupLE colKey countMap of
                  Just (startOffset, count) -> count + countUtf16CodeUnits (Source.slice slice (Range startOffset colKey))
                  Nothing -> countUtf16CodeUnits slice
        in (c, LineIndices $ Map.insert startRow (s, IntMap.insert colKey c countMap) map)

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
      | byte <= 0xef = Counter 2 (1 + sum) -- takes 2 bytes (1 utf16 cu)
      | byte <= 0xf7 = Counter 3 (2 + sum) -- takes 4 bytes (2 utf16 cu)
      | otherwise    = error "not valid utf8"
{-# INLINE countUtf16CodeUnits #-}

-- | The Source of the entire surrounding line (cached).
surroundingLine :: Source -> LineIndices -> Loc -> (Source, LineCache, LineIndices)
surroundingLine src li@(LineIndices map) loc@(Loc _ (Span (Pos start _) _)) =
  case Map.lookup start map of
    Just cache@(line, _) -> (line, cache, li)
    Nothing -> let cache = (line, mempty) in (line, cache, LineIndices (Map.insert start cache map))
  where
    line = Source.slice src range
    range = surroundingLineRange src loc

-- | The Range of the line surrounding the given location. (Takes advantage of
-- the fact that we already have the row information (where newlines are) from
-- tree-sitter.)
surroundingLineRange :: Source -> Loc -> Range
surroundingLineRange src (Loc (Range start _) (Span (Pos _ startCol) _)) = Range (start - startCol) lineEnd
  where
    lineEnd = maybe eof (start +) $ B.elemIndex lfChar remainingSource
    remainingSource = B.drop start (bytes src)

    lfChar = toEnum (ord '\n')
    eof = Source.length src
