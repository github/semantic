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
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Debug.Trace
import Data.List (intersperse)

type ClassName = String

data HTML =
  Break
  | Text String
  | Span (Maybe ClassName) String
  | Ul (Maybe ClassName) [HTML]
  | Dl (Maybe ClassName) [HTML]
  | Div (Maybe ClassName) [HTML]
  | Dt String
  deriving (Show, Eq)

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
  toMarkup Break = br
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
  deriving Eq

instance Show Row where
  show (Row left right) = "\n" ++ show left ++ " | " ++ show right

instance ToMarkup Row where
  toMarkup (Row left right) = (tr $ toMarkup left <> toMarkup right) <> string "\n"

instance ToMarkup Line where
  toMarkup EmptyLine = td (string "")
  toMarkup (Line html) = td . mconcat $ toMarkup <$> html

data Line =
  Line [HTML]
  | EmptyLine
  deriving Eq

unLine :: Line -> [HTML]
unLine EmptyLine = []
unLine (Line htmls) = htmls

instance Show Line where
  show (Line elements) = "[" ++ (concat . intersperse ", " $ show <$> elements) ++ "]"
  show EmptyLine = "EmptyLine"

instance Monoid Line where
 mempty = EmptyLine
 mappend EmptyLine EmptyLine = EmptyLine
 mappend EmptyLine (Line ys) = Line ys
 mappend (Line xs) EmptyLine = Line xs
 mappend (Line xs) (Line ys) = Line (xs <> ys)

instance Monoid Row where
  mempty = Row EmptyLine EmptyLine
  mappend (Row x1 y1) (Row x2 y2) = Row (x1 <> x2) (y1 <> y2)

diffToRows :: Diff a Info -> (Int, Int) -> String -> String -> ([Row], (Range, Range))
diffToRows (Free annotated) _ before after = annotatedToRows annotated before after
diffToRows (Pure (Insert term)) (previousIndex, _) _ after = (rowWithInsertedLine <$> afterLines, (Range previousIndex previousIndex, range))
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
    elements = (elementAndBreak $ Span (classify categories)) =<< actualLines (substring range source)

-- | Given an Annotated and before/after strings, returns a list of `Row`s representing the newline-separated diff.
annotatedToRows :: Annotated a (Info, Info) (Diff a Info) -> String -> String -> ([Row], (Range, Range))
annotatedToRows (Annotated (Info left _ leftCategories, Info right _ rightCategories) (Leaf _)) before after = (zipWithMaybe rowFromMaybeRows leftElements rightElements, (left, right))
  where
    leftElements = (elementAndBreak $ Span (classify leftCategories)) =<< actualLines (substring left before)
    rightElements = (elementAndBreak $ Span (classify rightCategories)) =<< actualLines (substring right after)

annotatedToRows (Annotated (Info left _ leftCategories, Info right _ rightCategories) (Indexed i)) before after = (rewrap <$> rows, ranges)
  where
    wrap _ EmptyLine = EmptyLine
    wrap f (Line elements) = Line [ f elements ]
    rewrap (Row left right) = Row (wrap (Ul $ classify leftCategories) left) (wrap (Ul $ classify rightCategories) right)
    ranges = (left, right)
    rows = appendRemainder $ foldl sumRows ([], starts ranges) i
    sources = (before, after)
    appendRemainder (rows, previousIndices) = reverse $ foldl adjoin2 [] $ rows ++ (contextRows (ends ranges) previousIndices sources)
    sumRows (rows, previousIndices) child = (allRows, ends childRanges)
      where
        separatorRows = contextRows (starts childRanges) previousIndices sources
        unadjoinedRows = rows ++ separatorRows ++ childRows
        allRows = reverse $ foldl adjoin2 [] unadjoinedRows
        (childRows, childRanges) = diffToRows child previousIndices before after

contextRows :: (Int, Int) -> (Int, Int) -> (String, String) -> [Row]
contextRows childIndices previousIndices sources = zipWithMaybe rowFromMaybeRows leftElements rightElements
  where
    leftElements = textElements (Range (fst previousIndices) (fst childIndices)) (fst sources)
    rightElements = textElements (Range (snd previousIndices) (snd childIndices)) (snd sources)

elementAndBreak :: (String -> HTML) -> String -> [HTML]
elementAndBreak _ "" = []
elementAndBreak constructor x | '\n' <- last x = [ constructor $ init x, Break ]
elementAndBreak constructor x = [ constructor x ]

textElements :: Range -> String -> [HTML]
textElements range source = (elementAndBreak Text) =<< actualLines s
  where s = substring range source

starts :: (Range , Range) -> (Int, Int)
starts (left, right) = (start left, start right)

ends :: (Range, Range) -> (Int, Int)
ends (left, right) = (end left, end right)

rowFromMaybeRows :: Maybe HTML -> Maybe HTML -> Row
rowFromMaybeRows a b = Row  (maybe EmptyLine (Line . (:[])) a) (maybe EmptyLine (Line . (:[])) b)

maybeLast :: [a] -> Maybe a
maybeLast list = listToMaybe $ reverse list

adjoin2 :: [Row] -> Row -> [Row]
adjoin2 [] row = [row]

adjoin2 rows (Row left' right') | Just _ <- openLine $ leftLines rows, Just _ <- openLine $ rightLines rows = zipWith Row lefts rights
  where lefts = adjoin2Lines (leftLines rows) left'
        rights = adjoin2Lines (rightLines rows) right'

adjoin2 rows (Row left' right') | Just _ <- openLine $ leftLines rows = Row EmptyLine right' : zipWith Row lefts rights
  where lefts = adjoin2Lines (leftLines rows) left'
        rights = rightLines rows

adjoin2 rows (Row left' right') | Just _ <- openLine $ rightLines rows = Row left' EmptyLine : zipWith Row lefts rights
  where lefts = leftLines rows
        rights = adjoin2Lines (rightLines rows) right'

adjoin2 (Row EmptyLine EmptyLine : rows) row = adjoin2 rows row

adjoin2 rows row = row : rows

leftLines :: [Row] -> [Line]
leftLines rows = left <$> rows
  where
    left (Row left _) = left

rightLines :: [Row] -> [Line]
rightLines rows = right <$> rows
  where
    right (Row _ right) = right

openLine :: [Line] -> Maybe Line
openLine [] = Nothing
openLine (EmptyLine : rest) = openLine rest
openLine (line : _) = case maybeLast $ unLine line of
  Just Break -> Nothing
  Just _ -> Just line
  Nothing -> Nothing

adjoin2Lines :: [Line] -> Line -> [Line]
adjoin2Lines [] line = [line]
adjoin2Lines (EmptyLine : xs) line | Just _ <- openLine xs = EmptyLine : adjoin2Lines xs line
adjoin2Lines (prev:rest) line = (prev <> line) : rest

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
  (l, lines') -> (case lines' of
                       [] -> [ l ]
                       _:lines' -> (l ++ "\n") : actualLines lines')
