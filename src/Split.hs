module Split where

import Prelude hiding (div, head, span)
import Diff
import Patch
import Term
import Syntax
import Control.Comonad.Cofree
import Range
import Control.Monad.Free
import Text.Blaze.Html
import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text as HText
import qualified OrderedMap as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Debug.Trace
import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import TextShow (showt)

type ClassName = T.Text

data HTML =
  Break
  | Text T.Text
  | Span (Maybe ClassName) T.Text
  | Ul (Maybe ClassName) [HTML]
  | Dl (Maybe ClassName) [HTML]
  | Div (Maybe ClassName) [HTML]
  | Dt T.Text
  deriving (Show, Eq)

classifyMarkup :: Maybe ClassName -> Markup -> Markup
classifyMarkup (Just className) element = element ! A.class_ (textValue className)
classifyMarkup _ element = element

toLi :: HTML -> Markup
toLi (Text s) = text s
toLi e = li $ toMarkup e

toDd :: HTML -> Markup
toDd (Text s) = text s
toDd e = dd $ toMarkup e

instance ToMarkup HTML where
  toMarkup Break = br
  toMarkup (Text s) = text s
  toMarkup (Span className s) = classifyMarkup className . span $ text s
  toMarkup (Ul className children) = classifyMarkup className . ul $ mconcat (toLi <$> children)
  toMarkup (Dl className children) = classifyMarkup className . dl $ mconcat (toDd <$> children)
  toMarkup (Div className children) = classifyMarkup className . div $ mconcat (toMarkup <$> children)
  toMarkup (Dt key) = dt $ text key

trace' :: Show a => a -> a
trace' a = traceShow a a

split :: Diff a Info -> T.Text -> T.Text -> IO T.Text
split diff before after = return . TL.toStrict . HText.renderHtml
  . docTypeHtml
    . ((head $ link ! A.rel "stylesheet" ! A.href "style.css") <>)
    . body
      . (table ! A.class_ (stringValue "diff")) $
        ((<>) (colgroup $ (col ! A.width (stringValue . show $ columnWidth)) <> col <> (col ! A.width (stringValue . show $ columnWidth)) <> col))
        . mconcat $ toMarkup <$> reverse numbered
  where
    rows = fst $ diffToRows diff (0, 0) before after
    numbered = foldl numberRows [] rows
    maxNumber = case numbered of
      [] -> 0
      ((x, _, y, _) : _) -> max x y

    digits :: Int -> Int
    digits n = let base = 10 :: Int in
      ceiling (log(fromIntegral n) / log(fromIntegral base) :: Double)

    columnWidth = max (20 + digits maxNumber * 8) 40

    numberRows :: [(Int, Line, Int, Line)] -> Row -> [(Int, Line, Int, Line)]
    numberRows [] (Row EmptyLine EmptyLine) = []
    numberRows [] (Row left@(Line _ _) EmptyLine) = [(1, left, 0, EmptyLine)]
    numberRows [] (Row EmptyLine right@(Line _ _)) = [(0, EmptyLine, 1, right)]
    numberRows [] (Row left right) = [(1, left, 1, right)]
    numberRows rows@((leftCount, _, rightCount, _):_) (Row EmptyLine EmptyLine) = (leftCount, EmptyLine, rightCount, EmptyLine):rows
    numberRows rows@((leftCount, _, rightCount, _):_) (Row left@(Line _ _) EmptyLine) = (leftCount + 1, left, rightCount, EmptyLine):rows
    numberRows rows@((leftCount, _, rightCount, _):_) (Row EmptyLine right@(Line _ _)) = (leftCount, EmptyLine, rightCount + 1, right):rows
    numberRows rows@((leftCount, _, rightCount, _):_) (Row left right) = (leftCount + 1, left, rightCount + 1, right):rows

normalizedDiffToRows :: Diff a Info -> T.Text -> T.Text -> [Row]
normalizedDiffToRows diff before after = fst $ diffToRows diff (0, 0) before after


data Row = Row Line Line
  deriving Eq

instance Show Row where
  show (Row left right) = "\n" ++ show left ++ " | " ++ show right

instance ToMarkup (Int, Line, Int, Line) where
  toMarkup (m, left, n, right) = tr $ toMarkup (m, left) <> toMarkup (n, right) <> string "\n"

instance ToMarkup (Int, Line) where
  toMarkup (_, EmptyLine) = numberTd "" <> toMarkup EmptyLine <> string "\n"
  toMarkup (num, line@(Line True _)) = td (string $ show num) ! A.class_ (stringValue "blob-num blob-num-replacement") <> toMarkup line <> string "\n"
  toMarkup (num, line@(Line _ _)) = numberTd (show num) <> toMarkup line <> string "\n"

numberTd :: String -> Html
numberTd "" = td mempty ! A.class_ (stringValue "blob-num blob-num-empty empty-cell")
numberTd s = td (string s) ! A.class_ (stringValue "blob-num")

codeTd :: Bool -> Maybe Html -> Html
codeTd _ Nothing = td mempty ! A.class_ (stringValue "blob-code blob-code-empty empty-cell")
codeTd True (Just el) = td el ! A.class_ (stringValue "blob-code blob-code-replacement")
codeTd _ (Just el) = td el ! A.class_ (stringValue "blob-code")

instance ToMarkup Line where
  toMarkup EmptyLine = codeTd False Nothing
  toMarkup (Line changed html) = codeTd changed . Just . mconcat $ toMarkup <$> html

data Line =
  Line Bool [HTML]
  | EmptyLine
  deriving Eq

unLine :: Line -> [HTML]
unLine EmptyLine = []
unLine (Line _ htmls) = htmls

isChanged :: Line -> Bool
isChanged EmptyLine = False
isChanged (Line isChanged _) = isChanged

instance Show Line where
  show (Line change elements) = show change ++ " [" ++ (concat . intersperse ", " $ show <$> elements) ++ "]"
  show EmptyLine = "EmptyLine"

instance Monoid Line where
 mempty = EmptyLine
 mappend EmptyLine EmptyLine = EmptyLine
 mappend EmptyLine (Line c ys) = Line c ys
 mappend (Line c xs) EmptyLine = Line c xs
 mappend (Line c1 xs) (Line c2 ys) = Line (c1 || c2) (xs <> ys)

instance Monoid Row where
  mempty = Row EmptyLine EmptyLine
  mappend (Row x1 y1) (Row x2 y2) = Row (x1 <> x2) (y1 <> y2)

diffToRows :: Diff a Info -> (Int, Int) -> T.Text -> T.Text -> ([Row], (Range, Range))
diffToRows (Free annotated) _ before after = annotatedToRows annotated before after
diffToRows (Pure (Insert term)) (previousIndex, _) _ after = (rowWithInsertedLine <$> afterLines, (Range previousIndex previousIndex, range))
  where
    (afterLines, range) = termToLines term after
    rowWithInsertedLine (Line _ elements) = Row EmptyLine $ Line True [ Div (Just "insert") elements ]
    rowWithInsertedLine EmptyLine = mempty
diffToRows (Pure (Delete term)) (_, previousIndex) before _ = (rowWithDeletedLine <$> lines, (range, Range previousIndex previousIndex))
  where
    (lines, range) = termToLines term before
    rowWithDeletedLine (Line _ elements) = Row (Line True [ Div (Just "delete") elements ]) EmptyLine
    rowWithDeletedLine EmptyLine = mempty
diffToRows (Pure (Replace a b)) _ before after = (replacedRows, (leftRange, rightRange))
  where
    replacedRows = zipWithMaybe rowFromMaybeRows (replace <$> leftElements) (replace <$> rightElements)
    replace = (:[]) . Div (Just "replace") . unLine
    rowFromMaybeRows :: Maybe [HTML] -> Maybe [HTML] -> Row
    rowFromMaybeRows a b = Row (maybe EmptyLine (Line True) a) (maybe EmptyLine (Line True) b)
    (leftElements, leftRange) = termToLines a before
    (rightElements, rightRange) = termToLines b after

-- | Takes a term and a `source` and returns a list of HTML lines
-- | and their range within `source`.
termToLines :: Term a Info -> T.Text -> ([Line], Range)
termToLines (Info range categories :< syntax) source = (rows syntax, range)
  where
    rows (Leaf _) = reverse $ foldl adjoin2Lines [] $ Line True . (:[]) <$> elements
    rows (Indexed i) = rewrapLineContentsIn Ul <$> childLines i
    rows (Fixed f) = rewrapLineContentsIn Ul <$> childLines f
    rows (Keyed k) = rewrapLineContentsIn Dl <$> childLines k

    rewrapLineContentsIn f (Line _ elements) = Line True [ f (classify categories) elements ]
    rewrapLineContentsIn _ EmptyLine = EmptyLine
    lineElements r s = Line True . (:[]) <$> textElements r s
    childLines i = appendRemainder $ foldl sumLines ([], start range) i
    appendRemainder (lines, previous) = reverse . foldl adjoin2Lines [] $ lines ++ lineElements (Range previous (end range)) source
    sumLines (lines, previous) child = (allLines, end childRange)
      where
        separatorLines = lineElements (Range previous $ start childRange) source
        unadjoinedLines = lines ++ separatorLines ++ childLines
        allLines = reverse $ foldl adjoin2Lines [] unadjoinedLines
        (childLines, childRange) = termToLines child source
    elements = (elementAndBreak $ Span (classify categories)) =<< actualLines (substring range source)

-- | Given an Annotated and before/after strings, returns a list of `Row`s representing the newline-separated diff.
annotatedToRows :: Annotated a (Info, Info) (Diff a Info) -> T.Text -> T.Text -> ([Row], (Range, Range))
annotatedToRows (Annotated (Info left leftCategories, Info right rightCategories) syntax) before after = (rows syntax, ranges)
  where
    rows (Leaf _) = zipWithMaybe rowFromMaybeRows leftElements rightElements
    rows (Indexed i) = rewrapRowContentsIn Ul <$> childRows i
    rows (Fixed f) = rewrapRowContentsIn Ul <$> childRows f
    rows (Keyed k) = rewrapRowContentsIn Dl <$> childRows (snd <$> Map.toList k)

    leftElements = (elementAndBreak $ Span (classify leftCategories)) =<< actualLines (substring left before)
    rightElements = (elementAndBreak $ Span (classify rightCategories)) =<< actualLines (substring right after)

    wrap _ EmptyLine = EmptyLine
    wrap f (Line c elements) = Line c [ f elements ]
    rewrapRowContentsIn f (Row left right) = Row (wrap (f $ classify leftCategories) left) (wrap (f $ classify rightCategories) right)
    ranges = (left, right)
    sources = (before, after)
    childRows = appendRemainder . foldl sumRows ([], starts ranges)
    appendRemainder (rows, previousIndices) = reverse . foldl adjoin2 [] $ rows ++ (contextRows (ends ranges) previousIndices sources)
    sumRows (rows, previousIndices) child = (allRows, ends childRanges)
      where
        separatorRows = contextRows (starts childRanges) previousIndices sources
        allRows = rows ++ separatorRows ++ childRows
        (childRows, childRanges) = diffToRows child previousIndices before after

contextRows :: (Int, Int) -> (Int, Int) -> (T.Text, T.Text) -> [Row]
contextRows childIndices previousIndices sources = zipWithMaybe rowFromMaybeRows leftElements rightElements
  where
    leftElements = textElements (Range (fst previousIndices) (fst childIndices)) (fst sources)
    rightElements = textElements (Range (snd previousIndices) (snd childIndices)) (snd sources)

elementAndBreak :: (T.Text -> HTML) -> T.Text -> [HTML]
elementAndBreak _ "" = []
elementAndBreak _ "\n" = [ Break ]
elementAndBreak constructor x | '\n' <- T.last x = [ constructor $ T.init x, Break ]
elementAndBreak constructor x = [ constructor x ]

textElements :: Range -> T.Text -> [HTML]
textElements range source = (elementAndBreak Text) =<< actualLines s
  where s = substring range source

starts :: (Range , Range) -> (Int, Int)
starts (left, right) = (start left, start right)

ends :: (Range, Range) -> (Int, Int)
ends (left, right) = (end left, end right)

rowFromMaybeRows :: Maybe HTML -> Maybe HTML -> Row
rowFromMaybeRows a b = Row (maybe EmptyLine (Line False . (:[])) a) (maybe EmptyLine (Line False . (:[])) b)

maybeLast :: [a] -> Maybe a
maybeLast list = listToMaybe $ reverse list

adjoin2 :: [Row] -> Row -> [Row]
adjoin2 [] row = [row]

adjoin2 rows (Row EmptyLine EmptyLine) = rows

adjoin2 (Row EmptyLine EmptyLine : rows) row = adjoin2 rows row

adjoin2 rows (Row left' right') | Just _ <- openLine $ leftLines rows, Just _ <- openLine $ rightLines rows = zipWith Row lefts rights
  where lefts = adjoin2Lines (leftLines rows) left'
        rights = adjoin2Lines (rightLines rows) right'

adjoin2 rows (Row left' right') | Just _ <- openLine $ leftLines rows = case right' of
  EmptyLine -> rest
  _ -> Row EmptyLine right' : rest
  where rest = zipWith Row lefts rights
        lefts = adjoin2Lines (leftLines rows) left'
        rights = rightLines rows

adjoin2 rows (Row left' right') | Just _ <- openLine $ rightLines rows = case left' of
  EmptyLine -> rest
  _ -> Row left' EmptyLine : rest
  where rest = zipWith Row lefts rights
        lefts = leftLines rows
        rights = adjoin2Lines (rightLines rows) right'

adjoin2 rows row = row : rows

leftLines :: [Row] -> [Line]
leftLines rows = left <$> rows
  where
    left (Row left _) = left

rightLines :: [Row] -> [Line]
rightLines rows = right <$> rows
  where
    right (Row _ right) = right

openElement :: HTML -> Maybe HTML
openElement Break = Nothing
openElement (Ul _ elements) = openElement =<< maybeLast elements
openElement (Dl _ elements) = openElement =<< maybeLast elements
openElement (Div _ elements) = openElement =<< maybeLast elements
openElement h = Just h

openLine :: [Line] -> Maybe Line
openLine [] = Nothing
openLine (EmptyLine : rest) = openLine rest
openLine (line : _) = const line <$> (openElement =<< (maybeLast $ unLine line))

adjoin2Lines :: [Line] -> Line -> [Line]
adjoin2Lines [] line = [line]
adjoin2Lines (EmptyLine : xs) line | Just _ <- openLine xs = EmptyLine : adjoin2Lines xs line
adjoin2Lines (prev:rest) line | Just _ <- openLine [ prev ] = (prev <> line) : rest
adjoin2Lines lines line = line : lines

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
classify = foldr (const . Just . (T.append "category-")) Nothing

actualLines :: T.Text -> [T.Text]
actualLines "" = [""]
actualLines lines = case T.break (== '\n') lines of
  (l, lines') -> case lines' of
                       "" -> [ l ]
                       lines' -> (T.snoc l '\n') : (actualLines . T.tail) lines'
