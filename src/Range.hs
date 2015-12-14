module Range where

data Range = Range { start :: Int, end :: Int }
  deriving (Eq, Show)

substring :: Range -> String -> String
substring range = take (end range - start range) . drop (start range)

totalRange :: [a] -> Range
totalRange list = Range 0 $ length list

offsetRange :: Int -> Range -> Range
offsetRange i (Range start end) = Range (i + start) (i + end)

instance Ord Range where
  a <= b = start a <= start b
