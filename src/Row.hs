module Row where

import Data.Functor.Both as Both
import Data.Monoid
import Line
import Prelude hiding (fst, snd)

-- | A row in a split diff, composed of a before line and an after line.
newtype Row a = Row { unRow :: Both (Line a) }
  deriving (Eq, Foldable, Functor, Show, Traversable)

makeRow :: Line a -> Line a -> Row a
makeRow a = Row . both a

unLeft :: Row a -> Line a
unLeft = fst . unRow

unRight :: Row a -> Line a
unRight = snd . unRow

isOpenRowBy :: Both (MaybeOpen a) -> Row a -> Bool
isOpenRowBy f = runBothWith (&&) . (isOpenLineBy <$> f <*>) . unRow

isClosedRowBy :: Both (MaybeOpen a) -> Row a -> Bool
isClosedRowBy f = not . runBothWith (||) . (isOpenLineBy <$> f <*>) . unRow

coalesceLinesBy :: MaybeOpen a -> Line a -> Line a -> [Line a]
coalesceLinesBy f line nextLine | isOpenLineBy f line = [line <> nextLine]
coalesceLinesBy _ line nextLine = [line, nextLine]

-- | Merge open lines and prepend closed lines (as determined by a pair of functions) onto a list of rows.
adjoinRowsBy :: Both (MaybeOpen a) -> Row a -> [Row a] -> [Row a]
adjoinRowsBy f row (nextRow : rows) = zipWithDefaults makeRow (pure EmptyLine) (coalesceLinesBy <$> f <*> unRow row <*> unRow nextRow) ++ rows

adjoinRowsBy _ row rows = row : rows
