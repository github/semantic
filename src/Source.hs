{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Source where

import Prologue hiding (uncons)
import qualified Data.Text as Text
import Data.String
import Numeric
import Range
import SourceSpan

-- | The source, oid, path, and Maybe SourceKind of a blob in a Git repo.
data SourceBlob = SourceBlob { source :: Source Char, oid :: String, path :: FilePath, blobKind :: Maybe SourceKind }
  deriving (Show, Eq)

-- | The contents of a source file, backed by a vector for efficient slicing.
newtype Source a = Source { unSource :: Text  }
  deriving (Eq, Show, Functor)

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

sourceBlob :: Source Char -> FilePath -> SourceBlob
sourceBlob source filepath = SourceBlob source Source.nullOid filepath (Just defaultPlainBlob)

-- | Map blobs with Nothing blobKind to empty blobs.
idOrEmptySourceBlob :: SourceBlob -> SourceBlob
idOrEmptySourceBlob blob = if isNothing (blobKind blob)
                           then blob { oid = nullOid, blobKind = Nothing }
                           else blob

nullOid :: String
nullOid = "0000000000000000000000000000000000000000"

-- | Return a Source from a list of items.
fromList :: [Char] -> Source Char
fromList = Source . Text.pack

-- | Return a Source of Chars from a Text.
fromText :: Text -> Source Char
fromText = Source

-- | Return a Source that contains a slice of the given Source.
slice :: Range -> Source Char -> Source Char
slice range = Source . Text.take (rangeLength range) . Text.drop (start range) . unSource

-- | Return a String with the contents of the Source.
toString :: Source Char -> String
toString = Text.unpack . unSource

-- | Return a text with the contents of the Source.
toText :: Source Char -> Text
toText = unSource

-- | Return the item at the given  index.
at :: Source Char -> Int -> Char
at = Text.index . unSource

-- | Remove the first item and return it with the rest of the source.
uncons :: Source Char -> Maybe (Char, Source Char)
uncons (Source vector) = if Text.null vector then Nothing else Just (Text.head vector, Source $ Text.tail vector)

-- | Split the source into the longest prefix of elements that do not satisfy the predicate and the rest without copying.
break :: (Char -> Bool) -> Source Char -> (Source Char, Source Char)
break predicate (Source vector) = let (start, remainder) = Text.break predicate vector in (Source start, Source remainder)

-- | Split the contents of the source after newlines.
actualLines :: Source Char -> [Source Char]
actualLines source | Text.null (unSource source) = [ source ]
actualLines source = case Source.break (== '\n') source of
  (l, lines') -> case uncons lines' of
    Nothing -> [ l ]
    Just (_, lines') -> (l <> fromList "\n") : actualLines lines'

-- | Compute the line ranges within a given range of a string.
actualLineRanges :: Range -> Source Char -> [Range]
actualLineRanges range = drop 1 . scanl toRange (Range (start range) (start range)) . actualLines . slice range
  where toRange previous string = Range (end previous) $ end previous + Text.length (unSource string)

-- | Compute the character range given a Source and a SourceSpan.
sourceSpanToRange :: Source Char -> SourceSpan -> Range
sourceSpanToRange source SourceSpan{..} = Range start end
  where start = sumLengths leadingRanges + column spanStart
        end = start + sumLengths (take (line spanEnd - line spanStart) remainingRanges) + (column spanEnd - column spanStart)
        (leadingRanges, remainingRanges) = splitAt (line spanStart) (actualLineRanges (Source.totalRange source) source)
        sumLengths = sum . fmap (\ Range{..} -> end - start)

-- | Return a range that covers the entire text.
totalRange :: Source Char -> Range
totalRange = Range 0 . Text.length . unSource

rangeToSourceSpan :: Source Char -> Range -> SourceSpan
rangeToSourceSpan source range@Range{} = SourceSpan startPos endPos
  where startPos = maybe (SourcePos 1 1) (toStartPos 1) (head lineRanges)
        endPos = toEndPos (Prologue.length lineRanges) (fromMaybe (rangeAt 0) (snd <$> unsnoc lineRanges))
        lineRanges = actualLineRanges range source
        toStartPos line range = SourcePos line (start range)
        toEndPos line range = SourcePos line (end range)

length :: Source Char -> Int
length = Text.length . unSource

null :: Source Char -> Bool
null = Text.null . unSource

instance Semigroup (Source Char) where
  Source a <> Source b = Source (a <> b)

instance Monoid (Source Char) where
  mempty = fromList []
  mappend = (<>)
