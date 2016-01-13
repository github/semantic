{-# LANGUAGE FlexibleInstances #-}
module Range where

import qualified Data.Text as T
import Control.Applicative ((<|>))
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)
import Data.Semigroup

-- | A half-open interval of integers, defined by start & end indices.
data Range = Range { start :: !Int, end :: !Int }
  deriving (Eq, Show)

rangeLength :: Range -> Int
rangeLength range = end range - start range

substring :: Range -> T.Text -> T.Text
substring range = T.take (rangeLength range) . T.drop (start range)

sublist :: Range -> [a] -> [a]
sublist range = take (rangeLength range) . drop (start range)

totalRange :: T.Text -> Range
totalRange t = Range 0 $ T.length t

offsetRange :: Int -> Range -> Range
offsetRange i (Range start end) = Range (i + start) (i + end)

rangesAndWordsFrom :: Int -> String -> [(Range, String)]
rangesAndWordsFrom _ "" = []
rangesAndWordsFrom startIndex string = fromMaybe [] $ take isWord <|> take isPunctuation <|> skip Char.isSpace
  where
    take predicate = match predicate $ \ parsed rest -> (Range startIndex $ endFor parsed, parsed) : rangesAndWordsFrom (endFor parsed) rest
    skip predicate = match predicate $ \ parsed rest -> rangesAndWordsFrom (endFor parsed) rest
    endFor parsed = startIndex + length parsed
    match predicate transform = case span predicate string of
      ([], _) -> Nothing
      (parsed, rest) -> Just $ transform parsed rest
    -- | Is this a word character?
    -- | Word characters are defined as in [Ruby’s `\p{Word}` syntax](http://ruby-doc.org/core-2.1.1/Regexp.html#class-Regexp-label-Character+Properties), i.e.:
    -- | > A member of one of the following Unicode general category _Letter_, _Mark_, _Number_, _Connector_Punctuation_
    isWord c = Char.isLetter c || Char.isNumber c || Char.isMark c || Char.generalCategory c == Char.ConnectorPunctuation
    isPunctuation c = not (Char.isSpace c || isWord c)

-- | Return Just the last index from a non-empty range, or if the range is empty, Nothing.
maybeLastIndex :: Range -> Maybe Int
maybeLastIndex (Range start end) | start == end = Nothing
maybeLastIndex (Range _ end) = Just $ end - 1

unionRange :: Range -> Range -> Range
unionRange (Range start1 end1) (Range start2 end2) = Range (min start1 start2) (max end1 end2)

unionRanges :: (Functor f, Foldable f) => f Range -> Range
unionRanges ranges = option (Range 0 0) id . foldl mappend mempty $ Option . Just <$> ranges

instance Semigroup Range where
  (<>) = unionRange

instance Ord Range where
  a <= b = start a <= start b
