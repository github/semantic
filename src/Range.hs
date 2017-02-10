module Range where

import qualified Data.Char as Char
import Data.List (span)
import Data.List.NonEmpty (nonEmpty)
import Data.Semigroup
import Data.String
import Prologue
import Test.LeanCheck

-- | A half-open interval of integers, defined by start & end indices.
data Range = Range { start :: Int, end :: Int }
  deriving (Eq, Show, Generic)

-- | Make a range at a given index.
rangeAt :: Int -> Range
rangeAt a = Range a a

-- | Return the length of the range.
rangeLength :: Range -> Int
rangeLength range = end range - start range

-- | Offset a range by a constant delta.
offsetRange :: Range -> Int -> Range
offsetRange a b = Range (start a + b) (end a + b)

-- | Break a string down into words and sequences of punctuation. Return a list
-- | strings with ranges, assuming that the first character in the string is
-- | at the given index.
rangesAndWordsFrom :: Int -> String -> [(Range, String)]
rangesAndWordsFrom _ "" = []
rangesAndWordsFrom startIndex string = fromMaybe [] $ take isWord <|> take isPunctuation <|> skip Char.isSpace
  where
    save parsed = (Range startIndex $ endFor parsed, parsed)
    take = parse (Just . save)
    skip = parse (const Nothing)
    endFor parsed = startIndex + length parsed
    parse transform predicate = case span predicate string of
      ([], _) -> Nothing
      (parsed, rest) -> Just . maybe identity (:) (transform parsed) $ rangesAndWordsFrom (endFor parsed) rest
    -- | Is this a word character?
    -- | Word characters are defined as in [Ruby’s `\p{Word}` syntax](http://ruby-doc.org/core-2.1.1/Regexp.html#class-Regexp-label-Character+Properties), i.e:.
    -- | > A member of one of the following Unicode general category _Letter_, _Mark_, _Number_, _Connector_Punctuation_
    isWord c = Char.isLetter c || Char.isNumber c || Char.isMark c || Char.generalCategory c == Char.ConnectorPunctuation
    isPunctuation c = not (Char.isSpace c || isWord c)

-- | Return Just the last index from a non-empty range, or if the range is empty, Nothing.
maybeLastIndex :: Range -> Maybe Int
maybeLastIndex (Range start end) | start == end = Nothing
maybeLastIndex (Range _ end) = Just $ end - 1

-- | Test two ranges for intersection.
intersectsRange :: Range -> Range -> Bool
intersectsRange range1 range2 = start range1 < end range2 && start range2 < end range1

-- Return the (possibly empty, possibly ill-formed) intersection of two ranges.
intersectionRange :: Range -> Range -> Range
intersectionRange range1 range2 = Range (max (start range1) (start range2)) (min (end range1) (end range2))

-- | Return a range that contains both the given ranges.
unionRange :: Range -> Range -> Range
unionRange (Range start1 end1) (Range start2 end2) = Range (min start1 start2) (max end1 end2)

-- | Return a range that contains all the ranges in a Foldable, or the passed Range if the Foldable is empty.
unionRangesFrom :: Foldable f => Range -> f Range -> Range
unionRangesFrom range = maybe range sconcat . nonEmpty . toList


-- Instances

instance Semigroup Range where
  a <> b = unionRange a b

instance Ord Range where
  a <= b = start a <= start b

instance Listable Range where
  tiers = cons2 Range
