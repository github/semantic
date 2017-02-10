{-# OPTIONS_GHC -funbox-strict-fields #-}
module Source where

import Prologue hiding (uncons)
import qualified Data.Text as Text
import Data.String
import Numeric
import Range
import SourceSpan

-- | The source, oid, path, and Maybe SourceKind of a blob in a Git repo.
data SourceBlob = SourceBlob { source :: Source, oid :: String, path :: FilePath, blobKind :: Maybe SourceKind }
  deriving (Show, Eq)

-- | The contents of a source file, represented as Text.
data Source = Source { sourceText :: !Text, sourceRange :: !Range }
  deriving (Eq, Show)

-- | The kind of a blob, along with it's file mode.
data SourceKind = PlainBlob Word32  | ExecutableBlob Word32 | SymlinkBlob Word32
  deriving (Show, Eq)

modeToDigits :: SourceKind -> String
modeToDigits (PlainBlob mode) = showOct mode ""
modeToDigits (ExecutableBlob mode) = showOct mode ""
modeToDigits (SymlinkBlob mode) = showOct mode ""


-- | The default plain blob mode
defaultPlainBlob :: SourceKind
defaultPlainBlob = PlainBlob 0o100644

emptySourceBlob :: FilePath -> SourceBlob
emptySourceBlob filepath = SourceBlob (Source.fromList "")  Source.nullOid filepath Nothing

sourceBlob :: Source -> FilePath -> SourceBlob
sourceBlob source filepath = SourceBlob source Source.nullOid filepath (Just defaultPlainBlob)

-- | Map blobs with Nothing blobKind to empty blobs.
idOrEmptySourceBlob :: SourceBlob -> SourceBlob
idOrEmptySourceBlob blob = if isNothing (blobKind blob)
                           then blob { oid = nullOid, blobKind = Nothing }
                           else blob

nullOid :: String
nullOid = "0000000000000000000000000000000000000000"

-- | Return a Source from a finite string.
fromList :: [Char] -> Source
fromList = fromText . Text.pack

-- | Return a Source of Chars from a Text.
fromText :: Text -> Source
fromText sourceText = Source sourceText sourceRange
  where sourceRange = Range 0 (Text.length sourceText)

-- | Return a Source that contains a slice of the given Source.
slice :: Range -> Source -> Source
slice range Source{..} = Source (Text.take (rangeLength range) (Text.drop (start range - start sourceRange) sourceText)) range

-- | Return a String with the contents of the Source.
toString :: Source -> String
toString = Text.unpack . sourceText

-- | Return a text with the contents of the Source.
toText :: Source -> Text
toText = sourceText

-- | Return the item at the given  index.
at :: Source -> Int -> Char
at = Text.index . sourceText

-- | Remove the first item and return it with the rest of the source.
uncons :: Source -> Maybe (Char, Source)
uncons Source{..} = if Text.null sourceText then Nothing else Just (Text.head sourceText, Source (Text.tail sourceText) (snd (divideRange sourceRange (start sourceRange + 1))))

-- | Split the source into the longest prefix of elements that do not satisfy the predicate and the rest without copying.
break :: (Char -> Bool) -> Source -> (Source, Source)
break predicate Source{..} = (Source initial initialRange, Source remainder remainderRange)
  where (initial, remainder) = Text.break predicate sourceText
        (initialRange, remainderRange) = divideRange sourceRange (start sourceRange + Text.length initial)

-- | Split the contents of the source after newlines.
actualLines :: Source -> [Source]
actualLines source | Text.null (sourceText source) = [ source ]
actualLines source = case Source.break (== '\n') source of
  (l, lines') -> case uncons lines' of
    Nothing -> [ l ]
    Just (_, lines') -> (l <> fromList "\n") : actualLines lines'

-- | Compute the line ranges within a given range of a string.
actualLineRanges :: Range -> Source -> [Range]
actualLineRanges range = drop 1 . scanl toRange (Range (start range) (start range)) . actualLines . slice range
  where toRange previous source = Range (end previous) $ end previous + Source.length source

-- | Compute the character range given a Source and a SourceSpan.
sourceSpanToRange :: Source -> SourceSpan -> Range
sourceSpanToRange source SourceSpan{..} = Range start end
  where start = sumLengths leadingRanges + column spanStart
        end = start + sumLengths (take (line spanEnd - line spanStart) remainingRanges) + (column spanEnd - column spanStart)
        (leadingRanges, remainingRanges) = splitAt (line spanStart) (actualLineRanges (Source.totalRange source) source)
        sumLengths = sum . fmap (\ Range{..} -> end - start)

-- | Return a range that covers the entire text.
totalRange :: Source -> Range
totalRange = sourceRange

rangeToSourceSpan :: Source -> Range -> SourceSpan
rangeToSourceSpan source range@Range{} = SourceSpan startPos endPos
  where startPos = maybe (SourcePos 1 1) (toStartPos 1) (head lineRanges)
        endPos = toEndPos (Prologue.length lineRanges) (fromMaybe (rangeAt 0) (snd <$> unsnoc lineRanges))
        lineRanges = actualLineRanges range source
        toStartPos line range = SourcePos line (start range)
        toEndPos line range = SourcePos line (end range)

length :: Source -> Int
length = rangeLength . sourceRange

null :: Source -> Bool
null = Text.null . sourceText

instance Semigroup Source where
  Source a ar <> Source b br = Source (a <> b) (ar <> br)

instance Monoid Source where
  mempty = fromList []
  mappend = (<>)
