module Range where

import qualified Data.Text as T
import Control.Applicative ((<|>))
import qualified Data.Char as Char

data Range = Range { start :: Int, end :: Int }
  deriving (Eq, Show)

substring :: Range -> T.Text -> T.Text
substring range = T.take (end range - start range) . T.drop (start range)

totalRange :: [a] -> Range
totalRange list = Range 0 $ length list

offsetRange :: Int -> Range -> Range
offsetRange i (Range start end) = Range (i + start) (i + end)

rangesAndWordsFrom :: Int -> T.Text -> [(Range, T.Text)]
rangesAndWordsFrom _ "" = []
rangesAndWordsFrom startIndex string = maybe [] id $ takeAndContinue <$> (word <|> punctuation) <|> skipAndContinue <$> space
  where
    word = parse isWord string
    punctuation = parse (not . isWordOrSpace) string
    space = parse Char.isSpace string
    takeAndContinue (parsed, rest) = (Range startIndex $ endFor parsed, parsed) : rangesAndWordsFrom (endFor parsed) rest
    skipAndContinue (parsed, rest) = rangesAndWordsFrom (endFor parsed) rest
    endFor parsed = startIndex + T.length parsed
    parse predicate string = case T.span predicate string of
      ("", _) -> Nothing
      (parsed, rest) -> Just (parsed, rest)
    isWordOrSpace c = Char.isSpace c || isWord c
    -- | Is this a word character?
    -- | Word characters are defined as in [Ruby’s `\p{Word}` syntax](http://ruby-doc.org/core-2.1.1/Regexp.html#class-Regexp-label-Character+Properties), i.e.:
    -- | > A member of one of the following Unicode general category _Letter_, _Mark_, _Number_, _Connector_Punctuation_
    isWord c = Char.isLetter c || Char.isNumber c || Char.isMark c || Char.generalCategory c == Char.ConnectorPunctuation


instance Ord Range where
  a <= b = start a <= start b
