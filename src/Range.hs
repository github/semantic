module Range where

import qualified Data.Char as Char

data Range = Range { start :: Int, end :: Int }
  deriving (Eq, Show)

substring :: Range -> String -> String
substring range = take (end range - start range) . drop (start range)

totalRange :: [a] -> Range
totalRange list = Range 0 $ length list

offsetRange :: Int -> Range -> Range
offsetRange i (Range start end) = Range (i + start) (i + end)

rangesAndWordsFrom :: Int -> String -> [(Range, String)]
rangesAndWordsFrom startIndex string = case break (not . isWord) string of
  ([], []) -> []
  ([], rest) | (whitespace, rest) <- break isWord rest -> rangesAndWordsFrom (startIndex + length whitespace) rest
  (word, rest) -> (Range startIndex $ startIndex + length word, word) : case break isWord rest of (whitespace, rest) -> rangesAndWordsFrom (startIndex + length word + length whitespace) rest
  where
    word string = case span isWord string of
      ([], _) -> Nothing
      (word, rest) -> Just (word, rest)
    punctuation string = case break (\ c -> isWord c || Char.isSpace c) string of
      ([], _) -> Nothing
      (punctuation, rest) -> Just (punctuation, rest)
    space string = case span Char.isSeparator of
      ([], _) -> Nothing
      (space, rest) -> Just (space, rest)
    -- | Is this a word character?
    -- | Word characters are defined as in [Ruby’s `\p{Word}` syntax](http://ruby-doc.org/core-2.1.1/Regexp.html#class-Regexp-label-Character+Properties), i.e.:
    -- | > A member of one of the following Unicode general category _Letter_, _Mark_, _Number_, _Connector_Punctuation_
    isWord c = Char.isLetter c || Char.isNumber c || Char.isMark c || Char.generalCategory c == Char.ConnectorPunctuation


instance Ord Range where
  a <= b = start a <= start b
