module Row where

import Control.Arrow
import Data.Functor.Both as Both
import Data.Monoid
import Line
import Prelude hiding (fst, snd)

-- | A row in a split diff, composed of a before line and an after line.
newtype Row a = Row { unRow :: Both (Line a) }
  deriving (Eq, Functor)

makeRow :: Line a -> Line a -> Row a
makeRow a = Row . both a

unLeft :: Row a -> Line a
unLeft = fst . unRow

unRight :: Row a -> Line a
unRight = snd . unRow

isOpenRowBy :: Both (MaybeOpen a) -> Row a -> Bool
isOpenRowBy f = runBothWith (&&) . (isOpenLineBy <$> f <*>) . unRow

-- | Merge open lines and prepend closed lines (as determined by a pair of functions) onto a list of rows.
adjoinRowsBy :: Both (MaybeOpen a) -> Row a -> [Row a] -> [Row a]
adjoinRowsBy _ (Row (Both (EmptyLine, EmptyLine))) rows = rows

adjoinRowsBy f row (nextRow : rows) | isOpenRowBy f row = Row ((<>) <$> unRow row <*> unRow nextRow) : rows

adjoinRowsBy f (Row (Both (EmptyLine, right))) (Row (Both (nextLeft, nextRight)) : rows) | isOpenLineBy (fst f) nextLeft = makeRow nextLeft right : adjoinRowsBy f (makeRow mempty nextRight) rows

adjoinRowsBy f (Row (Both (left, EmptyLine))) (Row (Both (nextLeft, nextRight)) : rows) | isOpenLineBy (snd f) nextRight = makeRow left nextRight : adjoinRowsBy f (makeRow nextLeft mempty) rows

adjoinRowsBy f (Row (Both (left, right))) (Row (Both (nextLeft, nextRight)) : rows) | isOpenLineBy (fst f) left = makeRow (left <> nextLeft) right : adjoinRowsBy f (makeRow mempty nextRight) rows

adjoinRowsBy f (Row (Both (left, right))) (Row (Both (nextLeft, nextRight)) : rows) | isOpenLineBy (snd f) right = makeRow left (right <> nextRight) : adjoinRowsBy f (makeRow nextLeft mempty) rows

adjoinRowsBy _ row rows = row : rows

instance Show a => Show (Row a) where
  show (Row (Both (left, right))) = "\n" ++ show left ++ " | " ++ show right

instance Applicative Row where
  pure = Row . pure . pure
  Row (Both (f, g)) <*> Row (Both (a, b)) = Row $ both (f <*> a) (g <*> b)
