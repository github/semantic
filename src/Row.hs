module Row where

import Control.Arrow
import Line

data Row a = Row { unLeft :: !(Line a), unRight :: !(Line a) }
  deriving (Eq, Functor)

unRow :: Row a -> (Line a, Line a)
unRow (Row a b) = (a, b)

wrapRowContents :: ([a] -> b) -> ([a] -> b) -> Row a -> Row b
wrapRowContents transformLeft transformRight (Row left right) = Row (wrapLineContents transformLeft left) (wrapLineContents transformRight right)

adjoinRowsBy :: MaybeOpen a -> MaybeOpen a -> [Row a] -> Row a -> [Row a]
adjoinRowsBy _ _ [] row = [row]

adjoinRowsBy f g rows (Row left' right') | Just _ <- openLineBy f $ unLeft <$> rows, Just _ <- openLineBy g $ unRight <$> rows = zipWith Row (lefts left') (rights right')
  where (lefts, rights) = adjoinLinesBy f *** adjoinLinesBy g $ unzip $ unRow <$> rows

adjoinRowsBy f _ rows (Row left' right') | Just _ <- openLineBy f $ unLeft <$> rows = case right' of
  EmptyLine -> rest
  _ -> Row EmptyLine right' : rest
  where rest = zipWith Row (lefts left') rights
        (lefts, rights) = first (adjoinLinesBy f) $ unzip $ unRow <$> rows

adjoinRowsBy _ g rows (Row left' right') | Just _ <- openLineBy g $ unRight <$> rows = case left' of
  EmptyLine -> rest
  _ -> Row left' EmptyLine : rest
  where rest = zipWith Row lefts (rights right')
        (lefts, rights) = second (adjoinLinesBy g) $ unzip $ unRow <$> rows

adjoinRowsBy _ _ rows row = row : rows


instance Show a => Show (Row a) where
  show (Row left right) = "\n" ++ show left ++ " | " ++ show right
