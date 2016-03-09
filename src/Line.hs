module Line where

import Data.Monoid
import qualified Data.Vector as Vector

-- | A line of items or an empty line.
data Line a =
  Line (Vector.Vector a)
  | EmptyLine
  deriving (Eq, Foldable, Functor, Show, Traversable)

-- | Create a line from a list of items.
makeLine :: [a] -> Line a
makeLine = Line . Vector.fromList

-- | Return a list of items from a line.
unLine :: Line a -> [a]
unLine EmptyLine = []
unLine (Line elements) = Vector.toList elements

-- | Transform the line by applying a function to a list of all the items in the
-- | line.
wrapLineContents :: ([a] -> b) -> Line a -> Line b
wrapLineContents _ EmptyLine = EmptyLine
wrapLineContents transform line = makeLine [ transform (unLine line) ]

-- | Return the first item in the Foldable, or Nothing if it's empty.
maybeFirst :: Foldable f => f a -> Maybe a
maybeFirst = foldr (const . Just) Nothing

-- | Is the final element of a line matched by the given predicate?
isOpenLineBy :: (a -> Bool) -> Line a -> Bool
isOpenLineBy f (Line vector) = Vector.null vector || f (Vector.last vector)
isOpenLineBy _ _ = True

-- | Coalesce a pair of lines if the first is matched by a predicate.
coalesceLinesBy :: (a -> Bool) -> Line a -> Line a -> [Line a]
coalesceLinesBy f line nextLine | isOpenLineBy f line = [line <> nextLine]
coalesceLinesBy _ line nextLine = [line, nextLine]

-- | Merge open lines and prepend closed lines, pushing empty lines through open ones.
adjoinLinesBy :: (a -> Bool) -> Line a -> [Line a] -> [Line a]
adjoinLinesBy f line (next:rest) | isOpenLineBy f line = line <> next : rest
adjoinLinesBy _ line lines = line : lines

instance Applicative Line where
  pure = makeLine . (:[])
  a <*> b = makeLine $ unLine a <*> unLine b

instance Monoid (Line a) where
  mempty = EmptyLine
  mappend EmptyLine line = line
  mappend line EmptyLine = line
  mappend (Line xs) (Line ys) = Line (xs <> ys)
