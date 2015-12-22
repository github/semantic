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
import Data.Monoid
import qualified Data.Set as Set
import Data.List (intercalate)

type ClassName = String

classifyMarkup :: Maybe ClassName -> Markup -> Markup
classifyMarkup (Just className) element = element ! A.class_ (stringValue className)
classifyMarkup _ element = element

split :: Diff a Info -> String -> String -> IO ByteString
split diff before after = return . renderHtml
  . docTypeHtml
    . ((head $ link ! A.rel (stringValue "stylesheet") ! A.href (stringValue "style.css")) <>)
    . body
      . (table ! A.class_ (stringValue "diff")) $
        ((colgroup $ (col ! A.width (stringValue . show $ columnWidth)) <> col <> (col ! A.width (stringValue . show $ columnWidth)) <> col) <>)
        . mconcat $ toMarkup <$> reverse numbered
  where
    rows = toRenderable <$> fst (splitDiffByLines diff (0, 0) (before, after))
    toRenderable (Row a b) = Row (Renderable . (,) before <$> a) (Renderable . (,) after <$> b)
    numbered = foldl numberRows [] rows
    maxNumber = case numbered of
      [] -> 0
      ((x, _, y, _) : _) -> max x y

    digits :: Int -> Int
    digits n = let base = 10 :: Int in
      ceiling (logBase (fromIntegral base) (fromIntegral n) :: Double)

    columnWidth = max (20 + digits maxNumber * 8) 40

    numberRows :: [(Int, Line a, Int, Line a)] -> Row a -> [(Int, Line a, Int, Line a)]
    numberRows [] (Row EmptyLine EmptyLine) = []
    numberRows [] (Row left@(Line _) EmptyLine) = [(1, left, 0, EmptyLine)]
    numberRows [] (Row EmptyLine right@(Line _)) = [(0, EmptyLine, 1, right)]
    numberRows [] (Row left right) = [(1, left, 1, right)]
    numberRows rows@((leftCount, _, rightCount, _):_) (Row EmptyLine EmptyLine) = (leftCount, EmptyLine, rightCount, EmptyLine):rows
    numberRows rows@((leftCount, _, rightCount, _):_) (Row left@(Line _) EmptyLine) = (leftCount + 1, left, rightCount, EmptyLine):rows
    numberRows rows@((leftCount, _, rightCount, _):_) (Row EmptyLine right@(Line _)) = (leftCount, EmptyLine, rightCount + 1, right):rows
    numberRows rows@((leftCount, _, rightCount, _):_) (Row left right) = (leftCount + 1, left, rightCount + 1, right):rows


data Row a = Row { unLeft :: Line a, unRight :: Line a }
  deriving (Eq, Functor)

instance Show a => Show (Row a) where
  show (Row left right) = "\n" ++ show left ++ " | " ++ show right

instance ToMarkup a => ToMarkup (Int, Line a, Int, Line a) where
  toMarkup (m, left, n, right) = tr $ toMarkup (m, left) <> toMarkup (n, right) <> string "\n"

instance ToMarkup a => ToMarkup (Int, Line a) where
  toMarkup (_, line@EmptyLine) = numberTd "" <> toMarkup line <> string "\n"
  -- toMarkup (num, line@(Line _)) = td (string $ show num) ! A.class_ (stringValue "blob-num blob-num-replacement") <> toMarkup line <> string "\n"
  toMarkup (num, line@(Line _)) = numberTd (show num) <> toMarkup line <> string "\n"

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

-- | A diff with only one side’s annotations.
type SplitDiff leaf annotation = Free (Annotated leaf annotation) (Term leaf annotation)

newtype Renderable a = Renderable (String, a)

instance ToMarkup f => ToMarkup (Renderable (Info, Syntax a (f, Range))) where
  toMarkup (Renderable (source, (Info range categories, syntax))) = classifyMarkup (classify categories) $ case syntax of
    Leaf _ -> span . string $ substring range source
    Indexed children -> ul . mconcat $ contentElements children
    Fixed children -> ul . mconcat $ contentElements children
    Keyed children -> dl . mconcat $ contentElements children
    where markupForSeparatorAndChild :: ToMarkup f => ([Markup], Int) -> (f, Range) -> ([Markup], Int)
          markupForSeparatorAndChild (rows, previous) child = (rows ++ [ string (substring (Range previous $ start $ snd child) source), toMarkup $ fst child ], end $ snd child)

          contentElements children = let (elements, previous) = foldl markupForSeparatorAndChild ([], start range) children in
            elements ++ [ string $ substring (Range previous $ end range) source ]

instance ToMarkup (Renderable (Term a Info)) where
  toMarkup (Renderable (source, term)) = fst $ cata (\ info@(Info range _) syntax -> (toMarkup $ Renderable (source, (info, syntax)), range)) term

instance ToMarkup (Renderable (SplitDiff a Info)) where
  toMarkup (Renderable (source, diff)) = fst $ iter (\ (Annotated info@(Info range _) syntax) -> (toMarkup $ Renderable (source, (info, syntax)), range)) $ toMarkupAndRange <$> diff
    where toMarkupAndRange :: Term a Info -> (Markup, Range)
          toMarkupAndRange term@(Info range _ :< _) = ((div ! A.class_ (stringValue "patch")) . toMarkup $ Renderable (source, term), range)

splitDiffByLines :: Diff a Info -> (Int, Int) -> (String, String) -> ([Row (SplitDiff a Info)], (Range, Range))
splitDiffByLines diff (prevLeft, prevRight) sources = case diff of
  Free (Annotated annotation syntax) -> (splitAnnotatedByLines sources (ranges annotation) (categories annotation) syntax, ranges annotation)
  Pure (Insert term) -> let (lines, range) = splitTermByLines term (snd sources) in
    (Row EmptyLine . fmap Pure <$> lines, (Range prevLeft prevLeft, range))
  Pure (Delete term) -> let (lines, range) = splitTermByLines term (fst sources) in
    (flip Row EmptyLine . fmap Pure <$> lines, (range, Range prevRight prevRight))
  Pure (Replace leftTerm rightTerm) -> let (leftLines, leftRange) = splitTermByLines leftTerm (fst sources)
                                           (rightLines, rightRange) = splitTermByLines rightTerm (snd sources) in
                                           (zipWithDefaults Row EmptyLine EmptyLine (fmap Pure <$> leftLines) (fmap Pure <$> rightLines), (leftRange, rightRange))
  where categories (Info _ left, Info _ right) = (left, right)
        ranges (Info left _, Info right _) = (left, right)

-- | Takes a term and a source and returns a list of lines and their range within source.
splitTermByLines :: Term a Info -> String -> ([Line (Term a Info)], Range)
splitTermByLines (Info range categories :< syntax) source = flip (,) range $ case syntax of
  Leaf a -> contextLines (:< Leaf a) range categories source
  Indexed children -> adjoinChildLines Indexed children
  Fixed children -> adjoinChildLines Fixed children
  Keyed children -> adjoinChildLines Keyed children
  where adjoin = reverse . foldl (adjoinLinesBy $ openTerm source) []
        adjoinChildLines constructor children = let (lines, previous) = foldl (childLines $ constructor mempty) ([], start range) children in
          adjoin $ lines ++ contextLines (:< constructor mempty) (Range previous $ end range) categories source

        childLines constructor (lines, previous) child = let (childLines, childRange) = splitTermByLines child source in
          (adjoin $ lines ++ contextLines (:< constructor) (Range previous $ start childRange) categories source ++ childLines, end childRange)

splitAnnotatedByLines :: (String, String) -> (Range, Range) -> (Set.Set Category, Set.Set Category) -> Syntax a (Diff a Info) -> [Row (SplitDiff a Info)]
splitAnnotatedByLines sources ranges categories syntax = case syntax of
  Leaf a -> contextRows (Leaf a) ranges categories sources
  Indexed children -> adjoinChildRows Indexed children
  Fixed children -> adjoinChildRows Fixed children
  Keyed children -> adjoinChildRows Keyed children
  where contextRows constructor ranges categories sources = zipWithDefaults Row EmptyLine EmptyLine (contextLines (Free . (`Annotated` constructor)) (fst ranges) (fst categories) (fst sources)) (contextLines (Free . (`Annotated` constructor)) (snd ranges) (snd categories) (snd sources))

        adjoin = reverse . foldl (adjoinRowsBy (openDiff $ fst sources) (openDiff $ snd sources)) []
        adjoinChildRows constructor children = let (rows, previous) = foldl (childRows $ constructor mempty) ([], starts ranges) children in
          adjoin $ rows ++ contextRows (constructor mempty) (makeRanges previous (ends ranges)) categories sources

        childRows constructor (rows, previous) child = let (childRows, childRanges) = splitDiffByLines child previous sources in
          (adjoin $ rows ++ contextRows constructor (makeRanges previous (starts childRanges)) categories sources ++ childRows, ends childRanges)

        starts (left, right) = (start left, start right)
        ends (left, right) = (end left, end right)
        makeRanges (leftStart, rightStart) (leftEnd, rightEnd) = (Range leftStart leftEnd, Range rightStart rightEnd)

contextLines :: (Info -> a) -> Range -> Set.Set Category -> String -> [Line a]
contextLines constructor range categories source = Line . (:[]) . constructor . (`Info` categories) <$> actualLineRanges range source

maybeLast :: Foldable f => f a -> Maybe a
maybeLast = foldl (flip $ const . Just) Nothing

adjoinRowsBy :: (a -> Maybe a) -> (a -> Maybe a) -> [Row a] -> Row a -> [Row a]
adjoinRowsBy _ _ [] row = [row]

adjoinRowsBy f g rows (Row left' right') | Just _ <- openLineBy f $ unLeft <$> rows, Just _ <- openLineBy g $ unRight <$> rows = zipWith Row lefts rights
  where lefts = adjoinLinesBy f (unLeft <$> rows) left'
        rights = adjoinLinesBy g (unRight <$> rows) right'

adjoinRowsBy f _ rows (Row left' right') | Just _ <- openLineBy f $ unLeft <$> rows = case right' of
  EmptyLine -> rest
  _ -> Row EmptyLine right' : rest
  where rest = zipWith Row lefts rights
        lefts = adjoinLinesBy f (unLeft <$> rows) left'
        rights = unRight <$> rows

adjoinRowsBy _ g rows (Row left' right') | Just _ <- openLineBy g $ unRight <$> rows = case left' of
  EmptyLine -> rest
  _ -> Row left' EmptyLine : rest
  where rest = zipWith Row lefts rights
        lefts = unLeft <$> rows
        rights = adjoinLinesBy g (unRight <$> rows) right'

adjoinRowsBy _ _ rows row = row : rows

openRange :: String -> Range -> Maybe Range
openRange source range = case (source !!) <$> maybeLastIndex range of
  Just '\n' -> Nothing
  _ -> Just range

openTerm :: String -> Term a Info -> Maybe (Term a Info)
openTerm source term@(Info range _ :< _) = const term <$> openRange source range

openDiff :: String -> SplitDiff a Info -> Maybe (SplitDiff a Info)
openDiff source diff@(Free (Annotated (Info range _) _)) = const diff <$> openRange source range
openDiff source diff@(Pure term) = const diff <$> openTerm source term

openLineBy :: (a -> Maybe a) -> [Line a] -> Maybe (Line a)
openLineBy _ [] = Nothing
openLineBy f (EmptyLine : rest) = openLineBy f rest
openLineBy f (line : _) = const line <$> (f =<< maybeLast (unLine line))

adjoinLinesBy :: (a -> Maybe a) -> [Line a] -> Line a -> [Line a]
adjoinLinesBy _ [] line = [line]
adjoinLinesBy f (EmptyLine : xs) line | Just _ <- openLineBy f xs = EmptyLine : adjoinLinesBy f xs line
adjoinLinesBy f (prev:rest) line | Just _ <- openLineBy f [ prev ] = (prev <> line) : rest
adjoinLinesBy _ lines line = line : lines

zipWithDefaults :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithDefaults f da db a b = take (max (length a) (length b)) $ zipWith f (a ++ repeat da) (b ++ repeat db)

classify :: Set.Set Category -> Maybe ClassName
classify categories = ("category-" ++) <$> maybeLast categories

actualLines :: String -> [String]
actualLines "" = [""]
actualLines lines = case break (== '\n') lines of
  (l, lines') -> case lines' of
                      [] -> [ l ]
                      _:lines' -> (l ++ "\n") : actualLines lines'

-- | Compute the line ranges within a given range of a string.
actualLineRanges :: Range -> String -> [Range]
actualLineRanges range = drop 1 . scanl toRange (Range (start range) (start range)) . actualLines . substring range
  where toRange previous string = Range (end previous) $ end previous + length string
