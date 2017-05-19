{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Source where

import qualified Data.ByteString as B
import Data.String (IsString(..))
import qualified Data.Text as T
import Language
import Numeric
import Range
import Prologue
import SourceSpan
import System.IO (FilePath)
import Test.LeanCheck

-- | The source, oid, path, and Maybe SourceKind of a blob.
data SourceBlob = SourceBlob
  { source :: Source -- ^ The UTF-8 encoded source text of the blob.
  , oid :: T.Text -- ^ The Git object ID (SHA-1) of the blob.
  , path :: FilePath -- ^ The file path to the blob.
  , blobKind :: Maybe SourceKind -- ^ The kind of blob, Nothing denotes a blob that doesn't exist (e.g. on one side of a diff for adding a new file or deleting a file).
  , blobLanguage :: Maybe Language -- ^ The language of this blob. Nothing denotes a langauge we don't support yet.
  } deriving (Show, Eq)

-- | The contents of a source file, represented as a ByteString.
newtype Source = Source { sourceText :: B.ByteString }
  deriving (Eq, IsString, Show)

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
emptySourceBlob filepath = SourceBlob Source.empty Source.nullOid filepath Nothing Nothing

nullBlob :: SourceBlob -> Bool
nullBlob SourceBlob{..} = oid == nullOid || Source.null source

nonExistentBlob :: SourceBlob -> Bool
nonExistentBlob SourceBlob{..} = isNothing blobKind

sourceBlob :: FilePath -> Maybe Language -> Source -> SourceBlob
sourceBlob filepath language source = SourceBlob source Source.nullOid filepath (Just defaultPlainBlob) language

-- | Map blobs with Nothing blobKind to empty blobs.
idOrEmptySourceBlob :: SourceBlob -> SourceBlob
idOrEmptySourceBlob blob = if isNothing (blobKind blob)
                           then blob { oid = nullOid, blobKind = Nothing }
                           else blob

nullOid :: T.Text
nullOid = "0000000000000000000000000000000000000000"

empty :: Source
empty = Source B.empty

-- | Return a Source from a ByteString.
fromText :: T.Text -> Source
fromText = Source . encodeUtf8

-- | Return a Source that contains a slice of the given Source.
slice :: Range -> Source -> Source
slice range = take . drop
  where drop = Source.drop (start range)
        take = Source.take (rangeLength range)

drop :: Int -> Source -> Source
drop i = Source . drop . sourceText
  where drop = B.drop i

take :: Int -> Source -> Source
take i = Source . take . sourceText
  where take = B.take i

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

-- | Compute the 'Range's of each line in a 'Source'.
actualLineRanges :: Source -> [Range]
actualLineRanges = Prologue.drop 1 . scanl toRange (Range 0 0) . actualLines
  where toRange previous string = Range (end previous) $ end previous + B.length (sourceText string)

-- | Compute the 'Range's of each line in a 'Range' of a 'Source'.
actualLineRangesWithin :: Range -> Source -> [Range]
actualLineRangesWithin range = Prologue.drop 1 . scanl toRange (Range (start range) (start range)) . actualLines . slice range
  where toRange previous string = Range (end previous) $ end previous + B.length (sourceText string)

-- | Compute the character range given a Source and a SourceSpan.
sourceSpanToRange :: Source -> SourceSpan -> Range
sourceSpanToRange source SourceSpan{..} = Range start end
  where start = pred (sumLengths leadingRanges + column spanStart)
        end = start + sumLengths (Prologue.take (line spanEnd - line spanStart) remainingRanges) + (column spanEnd - column spanStart)
        (leadingRanges, remainingRanges) = splitAt (pred (line spanStart)) (actualLineRanges source)
        sumLengths = sum . fmap rangeLength

rangeToSourceSpan :: Source -> Range -> SourceSpan
rangeToSourceSpan source range = SourceSpan startPos endPos
  where startPos = maybe (SourcePos 1 1) (toStartPos 1) (head lineRanges)
        endPos = toEndPos (Prologue.length lineRanges) (fromMaybe (Range 0 0) (snd <$> unsnoc lineRanges))
        lineRanges = actualLineRanges (slice range source)
        toStartPos line range = SourcePos line (succ (start range))
        toEndPos line range = SourcePos line (succ (end range))

-- | Return a 'Range' that covers the entire text.
totalRange :: Source -> Range
totalRange = Range 0 . B.length . sourceText

-- | Return a 'SourceSpan' that covers the entire text.
totalSpan :: Source -> SourceSpan
totalSpan source = SourceSpan (SourcePos 1 1) (SourcePos (Prologue.length ranges) (succ (end lastRange - start lastRange)))
  where ranges = actualLineRanges source
        Just lastRange = getLast (foldMap (Last . Just) ranges)

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
