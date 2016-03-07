module Source where

import Range
import qualified Data.Vector as Vector
import qualified Data.Text as T
import Foreign.C.Types

data SourceKind = PlainBlob CUInt  | ExecutableBlob CUInt | SymlinkBlob CUInt
  deriving (Show, Eq)

modeToDigits :: SourceKind -> String
modeToDigits (PlainBlob mode) = show mode
modeToDigits (ExecutableBlob mode) = show mode
modeToDigits (SymlinkBlob mode) = show mode

data SourceBlob = SourceBlob { source :: Source Char, oid :: String, path :: FilePath, blobKind :: Maybe SourceKind }
  deriving (Show, Eq)

-- | The contents of a source file, backed by a vector for efficient slicing.
newtype Source a = Source { getVector :: Vector.Vector a  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Return a Source from a list of items.
fromList :: [a] -> Source a
fromList = Source . Vector.fromList

-- | Return a Source of Chars from a Text.
fromText :: T.Text -> Source Char
fromText = Source . Vector.fromList . T.unpack

-- | Return a list of items with the contents of the Source.
toList :: Source a -> [a]
toList = Vector.toList . getVector

-- | Return a Source that contains a slice of the given Source.
slice :: Range -> Source a -> Source a
slice range = Source . Vector.slice (start range) (rangeLength range) . getVector

-- | Return a String with the contents of the Source.
toString :: Source Char -> String
toString = toList

-- | Return the item at the given  index.
at :: Source a -> Int -> a
at = (Vector.!) . getVector

-- | Test whether the source is empty.
null :: Source a -> Bool
null = Vector.null . getVector

-- | Prepend an item.
cons :: a -> Source a -> Source a
cons a = Source . Vector.cons a . getVector

-- | Remove the first item and return it with the rest of the source.
uncons :: Source a -> Maybe (a, Source a)
uncons (Source vector) = if Vector.null vector then Nothing else Just (Vector.head vector, Source $ Vector.tail vector)

-- | Split the source into the longest prefix of elements that do not satisfy the predicate and the rest without copying.
break :: (a -> Bool) -> Source a -> (Source a, Source a)
break predicate (Source vector) = let (start, remainder) = Vector.break predicate vector in (Source start, Source remainder)

-- | Concatenate two sources.
(++) :: Source a -> Source a -> Source a
(++) (Source a) = Source . (a Vector.++) . getVector

-- | Split the contents of the source after newlines.
actualLines :: Source Char -> [Source Char]
actualLines source | Source.null source = [ source ]
actualLines source = case Source.break (== '\n') source of
  (l, lines') -> case uncons lines' of
    Nothing -> [ l ]
    Just (_, lines') -> (l Source.++ fromList "\n") : actualLines lines'

-- | Compute the line ranges within a given range of a string.
actualLineRanges :: Range -> Source Char -> [Range]
actualLineRanges range = drop 1 . scanl toRange (Range (start range) (start range)) . actualLines . slice range
  where toRange previous string = Range (end previous) $ end previous + length string
