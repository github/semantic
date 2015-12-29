module Row where

import Line
import Data.Bifunctor

data Row a b = Row { unLeft :: Line a, unRight :: Line b }
  deriving Eq

instance Bifunctor Row where
  bimap f g (Row a b) = Row (f <$> a) (g <$> b)

bifmap :: Bifunctor f => (a -> b) -> f a a -> f b b
bifmap f = bimap f f

infixl 4 <$$>

(<$$>) :: Bifunctor f => (a -> b) -> f a a -> f b b
(<$$>) = bifmap

type Row' a = Row a a

wrapRowContents :: ([a] -> a') -> ([b] -> b') -> Row a b -> Row a' b'
wrapRowContents transformLeft transformRight (Row left right) = Row (wrapLineContents transformLeft left) (wrapLineContents transformRight right)

adjoinRowsBy :: MaybeOpen a -> MaybeOpen b -> [Row a b] -> Row a b -> [Row a b]
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


instance (Show a, Show b) => Show (Row a b) where
  show (Row left right) = "\n" ++ show left ++ " | " ++ show right
