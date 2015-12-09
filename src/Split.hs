module Split where

import Prelude hiding (div, head, span)
import Diff
import Patch
import Term
import Syntax

import Control.Comonad.Cofree
import Range
import Control.Monad.Free
import Data.ByteString.Lazy.Internal
import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8
import Data.Monoid
import qualified Data.Set as Set

type ClassName = String

data HTML =
  Text String
  | Span (Maybe ClassName) String
  | Ul (Maybe ClassName) [HTML]
  | Dl (Maybe ClassName) [HTML]
  | Div (Maybe ClassName) [HTML]
  | Dt String
  deriving (Show, Eq)

maybeFirstNewLine :: HTML -> Maybe HTML
maybeFirstNewLine text@(Text "") = Just text
maybeFirstNewLine text@(Text _) = Nothing
maybeFirstNewLine (Span _ _) = Nothing
maybeFirstNewLine (Dt _) = Nothing
maybeFirstNewLine (Ul _ elements) = getFirst $ mconcat $ map First $ map maybeFirstNewLine elements
maybeFirstNewLine (Dl _ elements) = getFirst $ mconcat $ map First $ map maybeFirstNewLine elements
maybeFirstNewLine (Div _ elements) = getFirst $ mconcat $ map First $ map maybeFirstNewLine elements

classifyMarkup :: Maybe ClassName -> Markup -> Markup
classifyMarkup (Just className) element = element ! A.class_ (stringValue className)
classifyMarkup _ element = element

toLi :: HTML -> Markup
toLi (Text s) = string s
toLi e = li $ toMarkup e

toDd :: HTML -> Markup
toDd (Text s) = string s
toDd e = dd $ toMarkup e

instance ToMarkup HTML where
  toMarkup (Text s) = string s
  toMarkup (Span className s) = classifyMarkup className . span $ string s
  toMarkup (Ul className children) = classifyMarkup className . ul $ mconcat (toLi <$> children)
  toMarkup (Dl className children) = classifyMarkup className . dl $ mconcat (toDd <$> children)
  toMarkup (Div className children) = classifyMarkup className . div $ mconcat (toMarkup <$> children)
  toMarkup (Dt key) = dt $ string key

split :: Diff a Info -> String -> String -> IO ByteString
split diff before after = return . renderHtml
  . docTypeHtml
    . ((head $ link ! A.rel (stringValue "stylesheet") ! A.href (stringValue "style.css")) <>)
    . body
      . (table ! A.class_ (stringValue "diff"))
        . mconcat $ toMarkup <$> (fst $ diffToRows diff (0, 0) before after)

data Row = Row Line Line
  deriving (Show, Eq)

instance ToMarkup Row where
  toMarkup (Row left right) = (tr $ toMarkup left <> toMarkup right) <> string "\n"

instance ToMarkup Line where
  toMarkup EmptyLine = td (string "")
  toMarkup (Line html) = td . mconcat $ toMarkup <$> html

data Line = Line { unLine :: [HTML] } | EmptyLine deriving (Show, Eq)

instance Monoid Line where
 mempty = EmptyLine
 mappend EmptyLine EmptyLine = EmptyLine
 mappend EmptyLine (Line ys) = Line ys
 mappend (Line xs) EmptyLine = Line xs
 mappend (Line xs) (Line ys) = Line (xs <> ys)

bimap :: ([HTML] -> [HTML]) -> ([HTML] -> [HTML]) -> Row -> Row
bimap _ _ (Row EmptyLine EmptyLine) = mempty
bimap f g (Row (Line a) (Line b)) = Row (Line $ f a) (Line $ g b)
bimap _ g (Row EmptyLine (Line b)) = Row EmptyLine (Line $ g b)
bimap f _ (Row (Line a) EmptyLine) = Row (Line $ f a) EmptyLine

instance Monoid Row where
  mempty = Row EmptyLine EmptyLine
  mappend (Row x1 y1) (Row x2 y2) = Row (x1 <> x2) (y1 <> y2)

diffToRows :: Diff a Info -> (Int, Int) -> String -> String -> ([Row], (Range, Range))
diffToRows (Free annotated) _ before after = annotatedToRows annotated before after
diffToRows (Pure (Insert term)) (previousIndex, _) _ after = (rowWithInsertedLine <$> afterLines, (range, Range previousIndex previousIndex))
  where
    (afterLines, range) = termToLines term after
    rowWithInsertedLine (Line elements) = Row EmptyLine $ Line [ Div (Just "insert") elements ]
    rowWithInsertedLine EmptyLine = mempty
diffToRows (Pure (Delete term)) (_, previousIndex) before _ = (rowWithDeletedLine <$> lines, (range, Range previousIndex previousIndex))
  where
    (lines, range) = termToLines term before
    rowWithDeletedLine (Line elements) = Row (Line [ Div (Just "delete") elements ]) EmptyLine
    rowWithDeletedLine EmptyLine = mempty
diffToRows (Pure (Replace a b)) _ before after = (replacedRows, (leftRange, rightRange))
  where
    replacedRows = zipWithMaybe rowFromMaybeRows (replace <$> leftElements) (replace <$> rightElements)
    replace = (:[]) . Div (Just "replace") . unLine
    rowFromMaybeRows :: Maybe [HTML] -> Maybe [HTML] -> Row
    rowFromMaybeRows a b = Row (maybe EmptyLine Line a) (maybe EmptyLine Line b)
    (leftElements, leftRange) = termToLines a before
    (rightElements, rightRange) = termToLines b after

-- | Takes a term and a `source` and returns a list of HTML lines
-- | and their range within `source`.
termToLines :: Term a Info -> String -> ([Line], Range)
termToLines (Info range _ categories :< syntax) source = (rows syntax, range)
  where
    rows (Leaf _) = Line . (:[]) <$> elements
    rows (Indexed i) = rewrapLineContentsInUl <$> childLines i

    rewrapLineContentsInUl (Line elements) = Line [ Ul (classify categories) elements ]
    rewrapLineContentsInUl EmptyLine = EmptyLine
    lineElements r s = Line . (:[]) <$> textElements r s
    childLines i = appendRemainder $ foldl sumLines ([], start range) i
    appendRemainder (lines, previous) = adjoinLines lines $ lineElements (Range previous (end range)) source
    sumLines (lines, previous) child = (allLines, end childRange)
      where
        separatorLines = lineElements (Range previous $ start childRange) source
        allLines = lines `adjoinLines` separatorLines `adjoinLines` childLines
        (childLines, childRange) = termToLines child source
    elements = Span (classify categories) <$> actualLines (substring range source)

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
        unajoinedRows = rows ++ separatorRows ++ childRows
        allRows = reverse $ foldl adjoin2 [] unajoinedRows
        (childRows, childRanges) = diffToRows child previousIndices before after

contextRows :: (Int, Int) -> (Int, Int) -> (String, String) -> [Row]
contextRows childIndices previousIndices sources = zipWithMaybe rowFromMaybeRows leftElements rightElements
  where
    leftElements = textElements (Range (fst previousIndices) (fst childIndices)) (fst sources)
    rightElements = textElements (Range (snd previousIndices) (snd childIndices)) (snd sources)

textElements range source = Text <$> actualLines (substring range source)

{-

["", ","]
"a", ""
"," ""
"b"

"a"
"", ","

-}

starts :: (Range , Range) -> (Int, Int)
starts (left, right) = (start left, start right)

ends :: (Range, Range) -> (Int, Int)
ends (left, right) = (end left, end right)

rowFromMaybeRows :: Maybe HTML -> Maybe HTML -> Row
rowFromMaybeRows a b = Row  (maybe EmptyLine (Line . (:[])) a) (maybe EmptyLine (Line . (:[])) b)

-- | Adjoin a list of rows onto an existing list of rows.
adjoinRows :: [Row] -> [Row] -> [Row]
adjoinRows [] rows = rows
adjoinRows rows [] = rows
adjoinRows accum (row : rows) = reverse (adjoin2 (reverse accum) row) ++ rows

adjoin2 :: [Row] -> Row -> [Row]
adjoin2 [] row = [row]
-- handle the case where we append a newline on both sides
adjoin2 rows (Row left@(Line (Text "" : _)) right) = Row left EmptyLine : zipWith Row lefts rights
  where
    lefts = leftLines rows
    rights = adjoin2Lines (rightLines rows) right
adjoin2 rows (Row left right@(Line ( Text "" : _))) = Row EmptyLine right : zipWith Row lefts rights
  where
    lefts = adjoin2Lines (leftLines rows) left
    rights = rightLines rows

adjoin2 (Row EmptyLine EmptyLine : init) row = adjoin2 init row

adjoin2 rows (Row left right) = zipWith Row lefts rights
  where
    lefts = adjoin2Lines (leftLines rows) left
    rights = adjoin2Lines (rightLines rows) right

leftLines :: [Row] -> [Line]
leftLines rows = left <$> rows
  where
    left (Row left _) = left

rightLines :: [Row] -> [Line]
rightLines rows = right <$> rows
  where
    right (Row _ right) = right

adjoin2Lines :: [Line] -> Line -> [Line]
adjoin2Lines [] line = [line]
adjoin2Lines (EmptyLine : xs) line = EmptyLine : (adjoin2Lines xs line)
adjoin2Lines (last:init) line = (last <> line) : init

adjoinLines :: [Line] -> [Line] -> [Line]
adjoinLines [] lines = lines
adjoinLines lines [] = lines
adjoinLines accum (line : lines) = init accum ++ [ last accum <> line ] ++ lines

{-
foo.bar([
  quux
]).baz
d()

foo.bar([ quux ]).baz
d()

"foo.bar([" "foo.bar([ quux ]).baz"
"  quux"    []
"]).baz"    []
"d()"       "d()"

"#include b"   "#include b"
"#include ..." []
"#include a"   "#include a"

-}

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
