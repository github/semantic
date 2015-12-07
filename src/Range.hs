module Range where

data Range = Range { start :: Int, end :: Int }
  deriving (Eq, Show)

substring :: Range -> String -> String
substring range = take (end range - start range) . drop (start range)

totalRange :: [a] -> Range
totalRange list = Range 0 $ length list

instance Ord Range where
  a <= b = start a <= start b
