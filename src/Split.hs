module Split where

import Diff

import Syntax

import Range
import Control.Monad.Free

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Rainbow

type ClassName = String

data HTML =
  Text String
  | Span (Maybe ClassName) String
  | Ul (Maybe ClassName) [HTML]
  | Dl (Maybe ClassName) [HTML]
  | Dt String
  deriving (Eq, Show)

split :: Diff a Info -> String -> String -> IO ByteString
split _ _ _ = return mempty

data Row = Row [HTML] [HTML]
  deriving (Eq, Show)

bimap :: ([HTML] -> HTML) -> ([HTML] -> HTML) -> Row -> Row
bimap f g (Row a b) = Row [ f a ] [ g b ]

instance Monoid Row where
  mempty = Row [] []
  mappend (Row x1 y1) (Row x2 y2) = Row (x1 <> x2) (y1 <> y2)

diffToRows :: Diff a Info -> String -> String -> ([Row], (Range, Range))
diffToRows (Free annotated) = annotatedToRows annotated

-- | Given an Annotated and before/after strings, returns a list of `Row`s representing the newline-separated diff.
annotatedToRows :: Annotated a (Info, Info) (Diff a Info) -> String -> String -> ([Row], (Range, Range))
annotatedToRows (Annotated (Info left _ leftCategories, Info right _ rightCategories) (Leaf _)) before after = (zipWithMaybe rowFromMaybeRows leftElements rightElements, (left, right))
  where
    leftElements = Span (classify leftCategories) <$> actualLines (substring left before)
    rightElements = Span (classify rightCategories) <$> actualLines (substring right after)

annotatedToRows (Annotated (Info left _ leftCategories, Info right _ rightCategories) (Indexed i)) before after = (bimap (Ul $ classify leftCategories) (Ul $ classify rightCategories) <$> rows, ranges)
  where
    ranges = (left, right)
    rows = appendRemainder $ foldl sumRows ([], starts ranges) i
    sources = (before, after)
    appendRemainder (rows, previousIndices) = adjoinRows rows $ contextRows (ends ranges) previousIndices sources
    sumRows (rows, previousIndices) child = (allRows, ends childRanges)
      where
        separatorRows = contextRows (starts childRanges) previousIndices sources
        allRows = rows `adjoinRows` separatorRows `adjoinRows` childRows
        (childRows, childRanges) = diffToRows child before after

contextRows :: (Int, Int) -> (Int, Int) -> (String, String) -> [Row]
contextRows childIndices previousIndices sources = zipWithMaybe rowFromMaybeRows leftElements rightElements
  where
    leftElements = Text <$> actualLines (substring (Range (fst previousIndices) (fst childIndices)) (fst sources))
    rightElements = Text <$> actualLines (substring (Range (snd previousIndices) (snd childIndices)) (snd sources))

starts :: (Range , Range) -> (Int, Int)
starts (left, right) = (start left, start right)

ends :: (Range, Range) -> (Int, Int)
ends (left, right) = (end left, end right)

rowFromMaybeRows :: Maybe HTML -> Maybe HTML -> Row
rowFromMaybeRows a b = Row (Maybe.maybeToList a) (Maybe.maybeToList b)

-- | Adjoin a list of rows onto an existing list of rows.
adjoinRows :: [Row] -> [Row] -> [Row]
adjoinRows [] rows = rows
adjoinRows rows [] = rows
adjoinRows accum (row : rows) = reverse (adjoin2 (reverse accum) row) ++ rows

adjoin2 :: [Row] -> Row -> [Row]
adjoin2 [] row = [row]
adjoin2 (Row [] [] : init) row = adjoin2 init row
adjoin2 (Row [] rights : Row lefts rights' : init) (Row xs ys) =
  Row [] (rights <> ys) : Row (lefts <> xs) rights' : init
adjoin2 (Row lefts [] : Row lefts' rights : init) (Row xs ys) =
  Row (lefts <> xs) [] : Row lefts' (rights <> ys) : init
adjoin2 (last:init) row = (last <> row) : init

zipWithMaybe :: (Maybe a -> Maybe b -> c) -> [a] -> [b] -> [c]
zipWithMaybe f la lb = take len $ zipWith f la' lb'
  where
    len = max (length la) (length lb)
    la' = (Just <$> la) ++ (repeat Nothing)
    lb' = (Just <$> lb) ++ (repeat Nothing)

classify :: Set.Set Category -> Maybe ClassName
classify = foldr (const . Just) Nothing

actualLines :: String -> [String]
actualLines "" = [""]
actualLines lines = case break (== '\n') lines of
  (l, lines') -> l : (case lines' of
                       [] -> []
                       _:lines' -> actualLines lines')
