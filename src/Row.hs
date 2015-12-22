module Row where

import Line

data Row a = Row { unLeft :: Line a, unRight :: Line a }
  deriving (Eq, Functor)

instance Show a => Show (Row a) where
  show (Row left right) = "\n" ++ show left ++ " | " ++ show right
