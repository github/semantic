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

-- | Given functions that determine whether an item is open, add a row to a
-- | first open, non-empty item in a list of rows, or add it as a new row.
adjoinRowsBy :: Both (MaybeOpen a) -> [Row a] -> Row a -> [Row a]
adjoinRowsBy _ [] row = [row]

adjoinRowsBy f rows (Row bothLines) | Both (Just _, Just _) <- openLineBy <$> f <*> Both.unzip (unRow <$> rows) = Both.zipWith makeRow $ both <*> bothLines
  where both = adjoinLinesBy <$> f <*> Both.unzip (unRow <$> rows)

adjoinRowsBy (Both (f, _)) rows (Row (Both (left', right'))) | Just _ <- openLineBy f $ unLeft <$> rows = case right' of
  EmptyLine -> rest
  _ -> makeRow EmptyLine right' : rest
  where rest = Prelude.zipWith makeRow (lefts left') rights
        (lefts, rights) = first (adjoinLinesBy f) . runBoth $ Both.unzip $ unRow <$> rows

adjoinRowsBy (Both (_, g)) rows (Row (Both (left', right'))) | Just _ <- openLineBy g $ unRight <$> rows = case left' of
  EmptyLine -> rest
  _ -> makeRow left' EmptyLine : rest
  where rest = Prelude.zipWith makeRow lefts (rights right')
        (lefts, rights) = second (adjoinLinesBy g) . runBoth $ Both.unzip $ unRow <$> rows

adjoinRowsBy _ rows row = row : rows

isOpenRowBy :: Both (MaybeOpen a) -> Row a -> Bool
isOpenRowBy f = runBothWith (&&) . (isOpenLineBy <$> f <*>) . unRow

-- | Merge open lines and prepend closed lines (as determined by a pair of functions) onto a list of rows.
adjoinRowsByR :: Both (MaybeOpen a) -> Row a -> [Row a] -> [Row a]
adjoinRowsByR _ (Row (Both (EmptyLine, EmptyLine))) rows = rows

adjoinRowsByR f row (nextRow : rows) | isOpenRowBy f row = Row ((<>) <$> unRow row <*> unRow nextRow) : rows

adjoinRowsByR f (Row (Both (EmptyLine, right))) (Row (Both (nextLeft, nextRight)) : rows) | isOpenLineBy (fst f) nextLeft = makeRow nextLeft right : adjoinRowsByR f (makeRow mempty nextRight) rows

adjoinRowsByR f (Row (Both (left, right))) (Row (Both (nextLeft, nextRight)) : rows) | isOpenLineBy (fst f) left = makeRow (left <> nextLeft) right : adjoinRowsByR f (makeRow mempty nextRight) rows

adjoinRowsByR f (Row (Both (left, EmptyLine))) (Row (Both (nextLeft, nextRight)) : rows) | isOpenLineBy (snd f) nextRight = makeRow left nextRight : adjoinRowsByR f (makeRow nextLeft mempty) rows

adjoinRowsByR f (Row (Both (left, right))) (Row (Both (nextLeft, nextRight)) : rows) | isOpenLineBy (snd f) right = makeRow left (right <> nextRight) : adjoinRowsByR f (makeRow nextLeft mempty) rows

adjoinRowsByR _ row rows = row : rows

instance Show a => Show (Row a) where
  show (Row (Both (left, right))) = "\n" ++ show left ++ " | " ++ show right

instance Applicative Row where
  pure = Row . pure . pure
  Row (Both (f, g)) <*> Row (Both (a, b)) = Row $ both (f <*> a) (g <*> b)
