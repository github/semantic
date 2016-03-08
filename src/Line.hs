{-# LANGUAGE FlexibleInstances #-}
module Line where

import Data.Maybe
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

-- | Return the last item in the Vector, or Nothing if it's empty.
maybeLast :: Vector.Vector a -> Maybe a
maybeLast vector = if Vector.null vector then Nothing else Just $ Vector.last vector

-- | A function that takes an input and returns a Maybe of the same type.
type MaybeOpen a = a -> Maybe a

isOpenLineBy :: MaybeOpen a -> Line a -> Bool
isOpenLineBy f (Line vector) = isJust (maybeLast vector >>= f)
isOpenLineBy _ _ = False

-- | Merge open lines and prepend closed lines, pushing empty lines through open ones.
adjoinLinesBy :: MaybeOpen a -> Line a -> [Line a] -> [Line a]
adjoinLinesBy f EmptyLine (next:rest) | isOpenLineBy f next = next : adjoinLinesBy f EmptyLine rest
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
