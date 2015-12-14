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

rangesOfWordsFrom :: Int -> String -> [Range]
rangesOfWordsFrom startIndex string = case break Char.isSpace string of
  ([], []) -> []
  ([], rest) -> rangesOfWordsAfterWhitespace startIndex rest
  (word, []) -> [ Range startIndex $ startIndex + length word ]
  (word, rest) -> (Range startIndex $ startIndex + length word) : rangesOfWordsAfterWhitespace (startIndex + length word) rest
  where
    rangesOfWordsAfterWhitespace startIndex string | (whitespace, rest) <- break (not . Char.isSpace) string = rangesOfWordsFrom (startIndex + length whitespace) rest

instance Ord Range where
  a <= b = start a <= start b
