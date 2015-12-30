module Line where

import qualified Data.Foldable as Foldable
import Data.Monoid
import qualified Data.Vector as Vector
import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5.Attributes as A

data Line a =
  Line (Vector.Vector a)
  | EmptyLine
  deriving (Eq, Functor, Foldable)

makeLine :: [a] -> Line a
makeLine = Line . Vector.fromList

unLine :: Line a -> [a]
unLine EmptyLine = []
unLine (Line elements) = Vector.toList elements

wrapLineContents :: ([a] -> b) -> Line a -> Line b
wrapLineContents _ EmptyLine = EmptyLine
wrapLineContents transform line = makeLine [ transform (unLine line) ]

maybeFirst :: Foldable f => f a -> Maybe a
maybeFirst = foldr (const . Just) Nothing

maybeLast :: Vector.Vector a -> Maybe a
maybeLast vector = if Vector.null vector then Nothing else Just $ Vector.last vector

type MaybeOpen a = a -> Maybe a

openLineBy :: MaybeOpen a -> [Line a] -> Maybe (Line a)
openLineBy _ [] = Nothing
openLineBy f (EmptyLine : rest) = openLineBy f rest
openLineBy f (line@(Line vector) : _) = const line <$> (f =<< maybeLast vector)

adjoinLinesBy :: MaybeOpen a -> [Line a] -> Line a -> [Line a]
adjoinLinesBy _ [] line = [line]
adjoinLinesBy f (EmptyLine : xs) line | Just _ <- openLineBy f xs = EmptyLine : adjoinLinesBy f xs line
adjoinLinesBy f (prev:rest) line | Just _ <- openLineBy f [ prev ] = (prev <> line) : rest
adjoinLinesBy _ lines line = line : lines

intersperse :: Foldable t => a -> t a -> [a]
intersperse separator elements = drop 1 $ foldr (\ each rest -> separator : each : rest) [] elements

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
