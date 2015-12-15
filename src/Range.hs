module Range where

import qualified Data.Text as T

data Range = Range { start :: Int, end :: Int }
  deriving (Eq, Show)

substring :: Range -> T.Text -> T.Text
substring range = T.take (end range - start range) . T.drop (start range)

totalRange :: [a] -> Range
totalRange list = Range 0 $ length list

instance Ord Range where
  a <= b = start a <= start b
