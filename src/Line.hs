module Line where

import Data.Monoid
import Data.List (intercalate)

data Line a =
  Line [a]
  | EmptyLine
  deriving (Eq, Functor)

unLine :: Line a -> [a]
unLine EmptyLine = []
unLine (Line elements) = elements

instance Show a => Show (Line a) where
  show (Line elements) = "[" ++ intercalate ", " (show <$> elements) ++ "]"
  show EmptyLine = "EmptyLine"

instance Monoid (Line a) where
  mempty = EmptyLine
  mappend EmptyLine line = line
  mappend line EmptyLine = line
  mappend (Line xs) (Line ys) = Line (xs <> ys)
