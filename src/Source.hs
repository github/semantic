{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Source where

import Prologue
import qualified Data.ByteString as B
import qualified Data.Text as T
import Numeric
import Range
import SourceSpan
import Test.LeanCheck

-- | The source, oid, path, and Maybe SourceKind of a blob in a Git repo.
data SourceBlob = SourceBlob { source :: Source, oid :: T.Text, path :: FilePath, blobKind :: Maybe SourceKind }
  deriving (Show, Eq)

-- | The contents of a source file, represented as a ByteString.
newtype Source = Source { sourceText :: B.ByteString }
  deriving (Eq, Show)

-- | The kind of a blob, along with it's file mode.
data SourceKind = PlainBlob Word32  | ExecutableBlob Word32 | SymlinkBlob Word32
  deriving (Show, Eq)

modeToDigits :: SourceKind -> Text
modeToDigits (PlainBlob mode) = toS $ showOct mode ""
modeToDigits (ExecutableBlob mode) = toS $ showOct mode ""
modeToDigits (SymlinkBlob mode) = toS $ showOct mode ""


-- | The default plain blob mode
defaultPlainBlob :: SourceKind
defaultPlainBlob = PlainBlob 0o100644

emptySourceBlob :: FilePath -> SourceBlob
emptySourceBlob filepath = SourceBlob Source.empty Source.nullOid filepath Nothing

sourceBlob :: Source -> FilePath -> SourceBlob
sourceBlob source filepath = SourceBlob source Source.nullOid filepath (Just defaultPlainBlob)

-- | Map blobs with Nothing blobKind to empty blobs.
idOrEmptySourceBlob :: SourceBlob -> SourceBlob
idOrEmptySourceBlob blob = if isNothing (blobKind blob)
                           then blob { oid = nullOid, blobKind = Nothing }
                           else blob

nullOid :: T.Text
nullOid = "0000000000000000000000000000000000000000"

empty :: Source
empty = Source B.empty

-- | Return a Source from a list of characters.
fromList :: [Word8] -> Source
fromList = Source . B.pack

-- | Return a Source from a ByteString.
fromText :: T.Text -> Source
fromText = Source . encodeUtf8

-- | Return a Source that contains a slice of the given Source.
slice :: Range -> Source -> Source
slice range = Source . take . drop . sourceText
  where drop = B.drop (start range)
        take = B.take (rangeLength range)

-- | Return the ByteString contained in the Source.
toText :: Source -> Text
toText = decodeUtf8 . sourceText

-- | Split the source into the longest prefix of elements that do not satisfy the predicate and the rest without copying.
break :: (Word8 -> Bool) -> Source -> (Source, Source)
break predicate (Source text) = let (start, remainder) = B.break predicate text in (Source start, Source remainder)

-- | Split the contents of the source after newlines.
actualLines :: Source -> [Source]
actualLines = fmap Source . actualLines' . sourceText
  where actualLines' text
          | B.null text = [ text ]
          | otherwise = case B.break (== toEnum (fromEnum '\n')) text of
            (l, lines') -> case B.uncons lines' of
              Nothing -> [ l ]
              Just (_, lines') -> (l <> B.singleton (toEnum (fromEnum '\n'))) : actualLines' lines'

-- | Compute the line ranges within a given range of a string.
actualLineRanges :: Range -> Source -> [Range]
actualLineRanges range = drop 1 . scanl toRange (Range (start range) (start range)) . actualLines . slice range
  where toRange previous string = Range (end previous) $ end previous + B.length (sourceText string)

-- | Compute the character range given a Source and a SourceSpan.
sourceSpanToRange :: Source -> SourceSpan -> Range
sourceSpanToRange source SourceSpan{..} = Range start end
  where start = sumLengths leadingRanges + column spanStart
        end = start + sumLengths (take (line spanEnd - line spanStart) remainingRanges) + (column spanEnd - column spanStart)
        (leadingRanges, remainingRanges) = splitAt (line spanStart) (actualLineRanges (Source.totalRange source) source)
        sumLengths = sum . fmap (\ Range{..} -> end - start)

-- | Return a range that covers the entire text.
totalRange :: Source -> Range
totalRange = Range 0 . B.length . sourceText

rangeToSourceSpan :: Source -> Range -> SourceSpan
rangeToSourceSpan source range@Range{} = SourceSpan startPos endPos
  where startPos = maybe (SourcePos 1 1) (toStartPos 1) (head lineRanges)
        endPos = toEndPos (Prologue.length lineRanges) (fromMaybe (rangeAt 0) (snd <$> unsnoc lineRanges))
        lineRanges = actualLineRanges range source
        toStartPos line range = SourcePos line (start range)
        toEndPos line range = SourcePos line (end range)

length :: Source -> Int
length = B.length . sourceText

null :: Source -> Bool
null = B.null . sourceText

instance Semigroup Source where
  Source a <> Source b = Source (a <> b)

instance Monoid Source where
  mempty = Source.empty
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
