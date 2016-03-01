{-# LANGUAGE FlexibleInstances #-}
module Line where

import qualified Data.Foldable as Foldable
import Data.Monoid
import qualified Data.Vector as Vector
import GHC.Generics
import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5.Attributes as A

-- | A line of items or an empty line.
data Line a =
  Line (Vector.Vector a)
  | EmptyLine
  deriving (Eq, Foldable, Functor, Generic, Traversable)

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

-- | Returns the first non-empty line in the list, or Nothing if the last item
-- | in the line doesn't pass the given MaybeOpen or if there are no non-empty
-- | lines.
openLineBy :: MaybeOpen a -> [Line a] -> Maybe (Line a)
openLineBy _ [] = Nothing
openLineBy f (EmptyLine : rest) = openLineBy f rest
openLineBy f (line@(Line vector) : _) = const line <$> (f =<< maybeLast vector)

-- | Given a function that determines whether an item is open, add a line to a
-- | first open, non-empty item in a list of lines, or add it as a new line.
adjoinLinesBy :: MaybeOpen a -> [Line a] -> Line a -> [Line a]
adjoinLinesBy _ [] line = [line]
adjoinLinesBy f (EmptyLine : xs) line | Just _ <- openLineBy f xs = EmptyLine : adjoinLinesBy f xs line
adjoinLinesBy f (prev:rest) line | Just _ <- openLineBy f [ prev ] = (prev <> line) : rest
adjoinLinesBy _ lines line = line : lines

-- | Create a list that contains all of the `a`s in `elements` separated by
-- | `separator`.
intersperse :: Foldable t => a -> t a -> [a]
intersperse separator elements = drop 1 $ foldr (\ each rest -> separator : each : rest) [] elements

-- | Create a list that contains all the items in the foldables in `elements`,
-- | where the contents of the different foldables are separated by the contents
-- | of `separator`.
intercalate :: (Foldable t, Foldable u) => t a -> u (t a) -> [a]
intercalate separator elements = concatMap Foldable.toList $ intersperse separator elements

instance Show a => Show (Line a) where
  show (Line elements) = "[" ++ intercalate ", " (show <$> elements) ++ "]"
  show EmptyLine = "EmptyLine"

instance Applicative Line where
  pure = makeLine . (:[])
  a <*> b = makeLine $ unLine a <*> unLine b

instance Monoid (Line a) where
  mempty = EmptyLine
  mappend EmptyLine line = line
  mappend line EmptyLine = line
  mappend (Line xs) (Line ys) = Line (xs <> ys)

instance ToMarkup a => ToMarkup (Bool, Int, Line a) where
  toMarkup (_, _, EmptyLine) = td mempty ! A.class_ (stringValue "blob-num blob-num-empty empty-cell") <> td mempty ! A.class_ (stringValue "blob-code blob-code-empty empty-cell") <> string "\n"
  toMarkup (hasChanges, num, Line contents)
    = td (string $ show num) ! A.class_ (stringValue $ if hasChanges then "blob-num blob-num-replacement" else "blob-num")
    <> td (mconcat . Vector.toList $ toMarkup <$> contents) ! A.class_ (stringValue $ if hasChanges then "blob-code blob-code-replacement" else "blob-code") <> string "\n"
