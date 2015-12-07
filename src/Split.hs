module Split where

import Diff
import Patch
import Term
import Syntax
import Control.Monad
import Control.Comonad.Cofree
import Range
import Control.Monad.Free

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Rainbow
import Data.ByteString.Char8 (pack)

type ClassName = String

data HTML =
  Text String
  | Span (Maybe ClassName) String
  | Ul (Maybe ClassName) [HTML]
  | Dl (Maybe ClassName) [HTML]
  | Dt String
  deriving Eq

showClassName :: Maybe ClassName -> String
showClassName (Just c) = " class='" ++ c ++ "'"
showClassName _ = ""

tag :: String -> String -> String
tag name contents = "<" ++ name ++ ">" ++ contents ++ "</" ++ name ++ ">"

table :: String -> String
table contents = "<table class='diff'>" ++ contents ++ "</table>"

li :: HTML -> String
li (Text string) = string
li html = tag "li" $ show html

dd :: HTML -> String
dd (Text string) = string
dd html = tag "dd" $ show html

instance Show HTML where
  show (Text string) = string
  show (Span className string) = "<span" ++ showClassName className ++ ">" ++ string ++ "</span>"
  show (Ul className children) = "<ul" ++ showClassName className ++ ">" ++ concat (li <$> children) ++ "</ul>"
  show (Dl className children) = "<dl" ++ showClassName className ++ ">" ++ concat (dd <$> children) ++ "</dl>"
  show (Dt key) = tag "dt" key

split :: Diff a Info -> String -> String -> IO ByteString
split diff before after = return . pack
  . tag "html"
    . (tag "head" "<link rel='stylesheet' href='style.css'/>" ++)
    . tag "body"
      . table
        . concat $ show <$> (fst $ diffToRows diff (0, 0) before after)

data Row = Row [HTML] [HTML]
  deriving Eq

instance Show Row where
  show (Row left right) = (tag "tr" $ (tag "td" . concat $ show <$> left) ++ (tag "td" . concat $ show <$> right)) ++ "\n"

bimap :: ([HTML] -> [HTML]) -> ([HTML] -> [HTML]) -> Row -> Row
bimap f g (Row a b) = Row (f a) (g b)

instance Monoid Row where
  mempty = Row [] []
  mappend (Row x1 y1) (Row x2 y2) = Row (x1 <> x2) (y1 <> y2)

diffToRows :: Diff a Info -> (Int, Int) -> String -> String -> ([Row], (Range, Range))
diffToRows (Free annotated) _ before after = annotatedToRows annotated before after
diffToRows (Pure (Insert term)) (previousIndex, _) _ after = (rowWithInsertedLine <$> afterLines, (range, Range previousIndex previousIndex))
  where
    (afterLines, range) = termToLines term after
    rowWithInsertedLine (Line elements) = Row [] elements
diffToRows (Pure (Delete term)) (_, previousIndex) before _ = (rowWithDeletedLine <$> lines, (range, Range previousIndex previousIndex))
  where
    (lines, range) = termToLines term before
    rowWithDeletedLine (Line elements) = Row elements []
diffToRows (Pure (Replace a b)) _ before after =  (zipWithMaybe rowFromMaybeRows (unLine <$> leftElements) (unLine <$> rightElements), (leftRange, rightRange))
  where
    rowFromMaybeRows :: Maybe [HTML] -> Maybe [HTML] -> Row
    rowFromMaybeRows a b = Row (join $ Maybe.maybeToList a) (join $ Maybe.maybeToList b)
    (leftElements, leftRange) = termToLines a before
    (rightElements, rightRange) = termToLines b after
    rowWithReplacedLine (Line elements) = Row elements []

newtype Line = Line { unLine :: [HTML] } deriving (Show, Eq)

instance Monoid Line where
 mempty = Line []
 mappend (Line xs) (Line ys) = Line (xs <> ys)

termToLines :: Term a Info -> String -> ([Line], Range)
termToLines (Info range _ categories :< syntax) source = (rows syntax, range)
  where
    rows (Leaf _) = Line . (:[]) <$> rightElements
    rows (Indexed i) = rewrapLineContentsInUl <$> childLines i

    rewrapLineContentsInUl (Line elements) = Line [ Ul (classify categories) elements ]
    lineElements r s = Line . (:[]) <$> textElements r s
    childLines i = appendRemainder $ foldl sumLines ([], start range) i
    appendRemainder (lines, previous) = adjoinLines lines $ lineElements (Range previous (end range)) source
    sumLines (lines, previous) child = (allLines, end childRange)
      where
        separatorLines = lineElements (Range previous $ start childRange) source
        allLines = lines `adjoinLines` separatorLines `adjoinLines` childLines
        (childLines, childRange) = termToLines child source
    rightElements = Span (classify categories) <$> actualLines (substring range source)

-- | Given an Annotated and before/after strings, returns a list of `Row`s representing the newline-separated diff.
annotatedToRows :: Annotated a (Info, Info) (Diff a Info) -> String -> String -> ([Row], (Range, Range))
annotatedToRows (Annotated (Info left _ leftCategories, Info right _ rightCategories) (Leaf _)) before after = (zipWithMaybe rowFromMaybeRows leftElements rightElements, (left, right))
  where
    leftElements = Span (classify leftCategories) <$> actualLines (substring left before)
    rightElements = Span (classify rightCategories) <$> actualLines (substring right after)

annotatedToRows (Annotated (Info left _ leftCategories, Info right _ rightCategories) (Indexed i)) before after = (bimap ((:[]) . Ul (classify leftCategories)) ((:[]) . Ul (classify rightCategories)) <$> rows, ranges)
  where
    ranges = (left, right)
    rows = appendRemainder $ foldl sumRows ([], starts ranges) i
    sources = (before, after)
    appendRemainder (rows, previousIndices) = adjoinRows rows $ contextRows (ends ranges) previousIndices sources
    sumRows (rows, previousIndices) child = (allRows, ends childRanges)
      where
        separatorRows = contextRows (starts childRanges) previousIndices sources
        allRows = rows `adjoinRows` separatorRows `adjoinRows` childRows
        (childRows, childRanges) = diffToRows child previousIndices before after

contextRows :: (Int, Int) -> (Int, Int) -> (String, String) -> [Row]
contextRows childIndices previousIndices sources = zipWithMaybe rowFromMaybeRows leftElements rightElements
  where
    leftElements = textElements (Range (fst previousIndices) (fst childIndices)) (fst sources)
    rightElements = textElements (Range (snd previousIndices) (snd childIndices)) (snd sources)

textElements range source = Text <$> actualLines (substring range source)

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

adjoinLines :: [Line] -> [Line] -> [Line]
adjoinLines [] lines = lines
adjoinLines lines [] = lines
adjoinLines accum (line : lines) = init accum ++ [ last accum <> line ] ++ lines

zipWithMaybe :: (Maybe a -> Maybe b -> c) -> [a] -> [b] -> [c]
zipWithMaybe f la lb = take len $ zipWith f la' lb'
  where
    len = max (length la) (length lb)
    la' = (Just <$> la) ++ (repeat Nothing)
    lb' = (Just <$> lb) ++ (repeat Nothing)

classify :: Set.Set Category -> Maybe ClassName
classify = foldr (const . Just . ("category-" ++)) Nothing

actualLines :: String -> [String]
actualLines "" = [""]
actualLines lines = case break (== '\n') lines of
  (l, lines') -> l : (case lines' of
                       [] -> []
                       _:lines' -> actualLines lines')
