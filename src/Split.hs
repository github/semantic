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
import Text.Blaze.Html
import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8
import qualified OrderedMap as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
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

    numberRows :: [(Int, Line HTML, Int, Line HTML)] -> Row HTML -> [(Int, Line HTML, Int, Line HTML)]
    numberRows [] (Row EmptyLine EmptyLine) = []
    numberRows [] (Row left@(Line _ _) EmptyLine) = [(1, left, 0, EmptyLine)]
    numberRows [] (Row EmptyLine right@(Line _ _)) = [(0, EmptyLine, 1, right)]
    numberRows [] (Row left right) = [(1, left, 1, right)]
    numberRows rows@((leftCount, _, rightCount, _):_) (Row EmptyLine EmptyLine) = (leftCount, EmptyLine, rightCount, EmptyLine):rows
    numberRows rows@((leftCount, _, rightCount, _):_) (Row left@(Line _ _) EmptyLine) = (leftCount + 1, left, rightCount, EmptyLine):rows
    numberRows rows@((leftCount, _, rightCount, _):_) (Row EmptyLine right@(Line _ _)) = (leftCount, EmptyLine, rightCount + 1, right):rows
    numberRows rows@((leftCount, _, rightCount, _):_) (Row left right) = (leftCount + 1, left, rightCount + 1, right):rows


data Row a = Row (Line a) (Line a)
  deriving Eq

instance Show a => Show (Row a) where
  show (Row left right) = "\n" ++ show left ++ " | " ++ show right

instance ToMarkup a => ToMarkup (Int, Line a, Int, Line a) where
  toMarkup (m, left, n, right) = tr $ toMarkup (m, left) <> toMarkup (n, right) <> string "\n"

instance ToMarkup a => ToMarkup (Int, Line a) where
  toMarkup (_, line@EmptyLine) = numberTd "" <> toMarkup line <> string "\n"
  toMarkup (num, line@(Line True _)) = td (string $ show num) ! A.class_ (stringValue "blob-num blob-num-replacement") <> toMarkup line <> string "\n"
  toMarkup (num, line@(Line _ _)) = numberTd (show num) <> toMarkup line <> string "\n"

numberTd :: String -> Html
numberTd "" = td mempty ! A.class_ (stringValue "blob-num blob-num-empty empty-cell")
numberTd s = td (string s) ! A.class_ (stringValue "blob-num")

codeTd :: Bool -> Maybe Html -> Html
codeTd _ Nothing = td mempty ! A.class_ (stringValue "blob-code blob-code-empty empty-cell")
codeTd True (Just el) = td el ! A.class_ (stringValue "blob-code blob-code-replacement")
codeTd _ (Just el) = td el ! A.class_ (stringValue "blob-code")

instance ToMarkup a => ToMarkup (Line a) where
  toMarkup EmptyLine = codeTd False Nothing
  toMarkup (Line changed html) = codeTd changed . Just . mconcat $ toMarkup <$> html

data Line a =
  Line Bool [HTML]
  | EmptyLine
  deriving Eq

unLine :: Line a -> [HTML]
unLine EmptyLine = []
unLine (Line _ elements) = elements

isChanged :: Line a -> Bool
isChanged EmptyLine = False
isChanged (Line isChanged _) = isChanged

instance Show a => Show (Line a) where
  show (Line change elements) = show change ++ " [" ++ (concat . intersperse ", " $ show <$> elements) ++ "]"
  show EmptyLine = "EmptyLine"

instance Monoid (Line a) where
 mempty = EmptyLine
 mappend EmptyLine EmptyLine = EmptyLine
 mappend EmptyLine (Line c ys) = Line c ys
 mappend (Line c xs) EmptyLine = Line c xs
 mappend (Line c1 xs) (Line c2 ys) = Line (c1 || c2) (xs <> ys)

instance Monoid (Row a) where
  mempty = Row EmptyLine EmptyLine
  mappend (Row x1 y1) (Row x2 y2) = Row (x1 <> x2) (y1 <> y2)

diffToRows :: Diff a Info -> (Int, Int) -> String -> String -> ([Row HTML], (Range, Range))
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
    rowFromMaybeRows :: Maybe [HTML] -> Maybe [HTML] -> Row HTML
    rowFromMaybeRows a b = Row (maybe EmptyLine (Line True) a) (maybe EmptyLine (Line True) b)
    (leftElements, leftRange) = termToLines a before
    (rightElements, rightRange) = termToLines b after

-- | Takes a term and a `source` and returns a list of HTML lines
-- | and their range within `source`.
termToLines :: Term a Info -> String -> ([Line HTML], Range)
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
annotatedToRows :: Annotated a (Info, Info) (Diff a Info) -> String -> String -> ([Row HTML], (Range, Range))
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

contextRows :: (Int, Int) -> (Int, Int) -> (String, String) -> [Row HTML]
contextRows childIndices previousIndices sources = zipWithMaybe rowFromMaybeRows leftElements rightElements
  where
    leftElements = textElements (Range (fst previousIndices) (fst childIndices)) (fst sources)
    rightElements = textElements (Range (snd previousIndices) (snd childIndices)) (snd sources)

elementAndBreak :: (String -> HTML) -> String -> [HTML]
elementAndBreak _ "" = []
elementAndBreak _ "\n" = [ Break ]
elementAndBreak constructor x | '\n' <- last x = [ constructor $ init x, Break ]
elementAndBreak constructor x = [ constructor x ]

textElements :: Range -> String -> [HTML]
textElements range source = (elementAndBreak Text) =<< actualLines s
  where s = substring range source

starts :: (Range , Range) -> (Int, Int)
starts (left, right) = (start left, start right)

ends :: (Range, Range) -> (Int, Int)
ends (left, right) = (end left, end right)

rowFromMaybeRows :: Maybe HTML -> Maybe HTML -> (Row HTML)
rowFromMaybeRows a b = Row (maybe EmptyLine (Line False . (:[])) a) (maybe EmptyLine (Line False . (:[])) b)

maybeLast :: [a] -> Maybe a
maybeLast list = listToMaybe $ reverse list

adjoin2 :: [Row a] -> Row a -> [Row a]
adjoin2 [] row = [row]

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

leftLines :: [Row a] -> [Line a]
leftLines rows = left <$> rows
  where
    left (Row left _) = left

rightLines :: [Row a] -> [Line a]
rightLines rows = right <$> rows
  where
    right (Row _ right) = right

openElement :: HTML -> Maybe HTML
openElement Break = Nothing
openElement (Ul _ elements) = openElement =<< maybeLast elements
openElement (Dl _ elements) = openElement =<< maybeLast elements
openElement (Div _ elements) = openElement =<< maybeLast elements
openElement h = Just h

openLine :: [Line HTML] -> Maybe (Line HTML)
openLine lines = openLineBy openElement lines

openLineBy :: (a -> Maybe a) -> [Line a] -> Maybe (Line a)
openLineBy _ [] = Nothing
openLineBy f (EmptyLine : rest) = openLineBy f rest
openLineBy f (line : _) = const line <$> (f =<< (maybeLast $ unLine line))

adjoin2Lines :: [Line HTML] -> Line HTML -> [Line HTML]
adjoin2Lines = adjoin2LinesBy openElement

adjoin2LinesBy :: (a -> Maybe a) -> [Line a] -> Line a -> [Line a]
adjoin2LinesBy _ [] line = [line]
adjoin2LinesBy f (EmptyLine : xs) line | Just _ <- openLineBy f xs = EmptyLine : adjoin2LinesBy f xs line
adjoin2LinesBy f (prev:rest) line | Just _ <- openLineBy f [ prev ] = (prev <> line) : rest
adjoin2LinesBy _ lines line = line : lines

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
