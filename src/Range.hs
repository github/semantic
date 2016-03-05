{-# LANGUAGE FlexibleInstances #-}
module Range where

import qualified Data.Text as T
import Control.Applicative ((<|>))
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)
import Data.Option

-- | A half-open interval of integers, defined by start & end indices.
data Range = Range { start :: !Int, end :: !Int }
  deriving (Eq, Show)

-- | Make a range at a given index.
rangeAt :: Int -> Range
rangeAt a = Range a a

-- | Return the length of the range.
rangeLength :: Range -> Int
rangeLength range = end range - start range

-- | Return the portion of the text identified by the given range.
substring :: Range -> T.Text -> T.Text
substring range = T.take (rangeLength range) . T.drop (start range)

-- | Return the portion of the list identified by the given range.
sublist :: Range -> [a] -> [a]
sublist range = take (rangeLength range) . drop (start range)

-- | Return a range that covers the entire text.
totalRange :: T.Text -> Range
totalRange t = Range 0 $ T.length t

-- | Return a range that has its start and end offset by the given amount.
offsetRange :: Int -> Range -> Range
offsetRange i (Range start end) = Range (i + start) (i + end)

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
      (parsed, rest) -> Just $ maybe id (:) (transform parsed) $ rangesAndWordsFrom (endFor parsed) rest
    -- | Is this a word character?
    -- | Word characters are defined as in [Ruby’s `\p{Word}` syntax](http://ruby-doc.org/core-2.1.1/Regexp.html#class-Regexp-label-Character+Properties), i.e.:
    -- | > A member of one of the following Unicode general category _Letter_, _Mark_, _Number_, _Connector_Punctuation_
    isWord c = Char.isLetter c || Char.isNumber c || Char.isMark c || Char.generalCategory c == Char.ConnectorPunctuation
    isPunctuation c = not (Char.isSpace c || isWord c)

-- | Return Just the last index from a non-empty range, or if the range is empty, Nothing.
maybeLastIndex :: Range -> Maybe Int
maybeLastIndex (Range start end) | start == end = Nothing
maybeLastIndex (Range _ end) = Just $ end - 1

-- | Return a range that contains both the given ranges.
unionRange :: Range -> Range -> Range
unionRange (Range start1 end1) (Range start2 end2) = Range (min start1 start2) (max end1 end2)

-- | Return a range that contains all the ranges in a Foldable, or Range 0 0 if it’s empty.
unionRanges :: (Functor f, Foldable f) => f Range -> Range
unionRanges = unionRangesFrom (Range 0 0)

-- | Return a range that contains all the ranges in a Foldable, or the passed Range if the Foldable is empty.
unionRangesFrom :: Foldable f => Range -> f Range -> Range
unionRangesFrom range = fromMaybe range . maybeConcat

-- | Return Just the union of all the ranges in a Foldable, or else Nothing.
maybeUnionRanges :: (Functor f, Foldable f) => f Range -> Maybe Range
maybeUnionRanges ranges = getOption $ foldl mappend mempty $ Option . Just <$> ranges

instance Monoid (Option Range) where
  mempty = Option Nothing
  mappend (Option (Just a)) (Option (Just b)) = Option (Just (unionRange a b))
  mappend a@(Option (Just _)) _ = a
  mappend _ b@(Option (Just _)) = b
  mappend _ _ = mempty

instance Ord Range where
  a <= b = start a <= start b
