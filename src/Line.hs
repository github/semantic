module Line where

import Data.Monoid
import Data.List (intercalate)
import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5.Attributes as A

data Line a =
  Line [a]
  | EmptyLine
  deriving (Eq, Functor, Foldable)

unLine :: Line a -> [a]
unLine EmptyLine = []
unLine (Line elements) = elements

maybeLast :: Foldable f => f a -> Maybe a
maybeLast = foldl (flip $ const . Just) Nothing

openLineBy :: (a -> Maybe a) -> [Line a] -> Maybe (Line a)
openLineBy _ [] = Nothing
openLineBy f (EmptyLine : rest) = openLineBy f rest
openLineBy f (line : _) = const line <$> (f =<< maybeLast (unLine line))

adjoinLinesBy :: (a -> Maybe a) -> [Line a] -> Line a -> [Line a]
adjoinLinesBy _ [] line = [line]
adjoinLinesBy f (EmptyLine : xs) line | Just _ <- openLineBy f xs = EmptyLine : adjoinLinesBy f xs line
adjoinLinesBy f (prev:rest) line | Just _ <- openLineBy f [ prev ] = (prev <> line) : rest
adjoinLinesBy _ lines line = line : lines

instance Show a => Show (Line a) where
  show (Line elements) = "[" ++ intercalate ", " (show <$> elements) ++ "]"
  show EmptyLine = "EmptyLine"

instance Monoid (Line a) where
  mempty = EmptyLine
  mappend EmptyLine line = line
  mappend line EmptyLine = line
  mappend (Line xs) (Line ys) = Line (xs <> ys)

instance ToMarkup a => ToMarkup (Bool, Int, Line a) where
  toMarkup (_, _, EmptyLine) = td mempty ! A.class_ (stringValue "blob-num blob-num-empty empty-cell") <> td mempty ! A.class_ (stringValue "blob-code blob-code-empty empty-cell") <> string "\n"
  toMarkup (hasChanges, num, Line contents)
    = td (string $ show num) ! A.class_ (stringValue $ if hasChanges then "blob-num blob-num-replacement" else "blob-num")
    <> td (mconcat $ toMarkup <$> contents) ! A.class_ (stringValue $ if hasChanges then "blob-code blob-code-replacement" else "blob-code") <> string "\n"
