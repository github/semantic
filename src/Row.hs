module Row where

import Line

data Row a = Row { unLeft :: Line a, unRight :: Line a }
  deriving (Eq, Functor)

wrapRowContents :: ([a] -> b) -> Row a -> Row b
wrapRowContents transform (Row left right) = Row (wrapLineContents transform left) (wrapLineContents transform right)

adjoinRowsBy :: (a -> Maybe a) -> (a -> Maybe a) -> [Row a] -> Row a -> [Row a]
adjoinRowsBy _ _ [] row = [row]

adjoinRowsBy f g rows (Row left' right') | Just _ <- openLineBy f $ unLeft <$> rows, Just _ <- openLineBy g $ unRight <$> rows = zipWith Row lefts rights
  where lefts = adjoinLinesBy f (unLeft <$> rows) left'
        rights = adjoinLinesBy g (unRight <$> rows) right'

adjoinRowsBy f _ rows (Row left' right') | Just _ <- openLineBy f $ unLeft <$> rows = case right' of
  EmptyLine -> rest
  _ -> Row EmptyLine right' : rest
  where rest = zipWith Row lefts rights
        lefts = adjoinLinesBy f (unLeft <$> rows) left'
        rights = unRight <$> rows

adjoinRowsBy _ g rows (Row left' right') | Just _ <- openLineBy g $ unRight <$> rows = case left' of
  EmptyLine -> rest
  _ -> Row left' EmptyLine : rest
  where rest = zipWith Row lefts rights
        lefts = unLeft <$> rows
        rights = adjoinLinesBy g (unRight <$> rows) right'

adjoinRowsBy _ _ rows row = row : rows


instance Show a => Show (Row a) where
  show (Row left right) = "\n" ++ show left ++ " | " ++ show right
