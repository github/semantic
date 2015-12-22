module Line where

import Data.Monoid
import Data.List (intercalate)
import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5.Attributes as A

data Line a =
  Line [a]
  | EmptyLine
  deriving (Eq, Functor)

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

instance ToMarkup a => ToMarkup (Int, Line a) where
  toMarkup (_, line@EmptyLine) = numberTd "" <> toMarkup line <> string "\n"
  -- toMarkup (num, line@(Line _)) = td (string $ show num) ! A.class_ (stringValue "blob-num blob-num-replacement") <> toMarkup line <> string "\n"
  toMarkup (num, line@(Line _)) = numberTd (show num) <> toMarkup line <> string "\n"
instance ToMarkup a => ToMarkup (Bool, Int, Line a) where
  toMarkup (_, _, line@EmptyLine) = numberTd "" <> toMarkup line <> string "\n"
  toMarkup (True, num, line@(Line _)) = td (string $ show num) ! A.class_ (stringValue "blob-num blob-num-replacement") <> toMarkup line <> string "\n"
  toMarkup (_, num, line@(Line _)) = numberTd (show num) <> toMarkup line <> string "\n"

numberTd :: String -> Html
numberTd "" = td mempty ! A.class_ (stringValue "blob-num blob-num-empty empty-cell")
numberTd s = td (string s) ! A.class_ (stringValue "blob-num")

codeTd :: Bool -> Maybe Html -> Html
codeTd _ Nothing = td mempty ! A.class_ (stringValue "blob-code blob-code-empty empty-cell")
codeTd True (Just el) = td el ! A.class_ (stringValue "blob-code blob-code-replacement")
codeTd False (Just el) = td el ! A.class_ (stringValue "blob-code")

instance ToMarkup a => ToMarkup (Line a) where
  toMarkup EmptyLine = codeTd False Nothing
  toMarkup (Line contents) = codeTd False . Just . mconcat $ toMarkup <$> contents
