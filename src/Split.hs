module Split where

import Prelude hiding (div, head, span)
import Diff
import Line
import Row
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
import Source hiding ((++))

type ClassName = String

classifyMarkup :: Foldable f => f String -> Markup -> Markup
classifyMarkup categories element = maybe element ((element !) . A.class_ . stringValue . ("category-" ++)) $ maybeFirst categories

split :: Eq a => Diff a Info -> Source Char -> Source Char -> IO ByteString
split diff before after = return . renderHtml
  . docTypeHtml
    . ((head $ link ! A.rel (stringValue "stylesheet") ! A.href (stringValue "style.css")) <>)
    . body
      . (table ! A.class_ (stringValue "diff")) $
        ((colgroup $ (col ! A.width (stringValue . show $ columnWidth)) <> col <> (col ! A.width (stringValue . show $ columnWidth)) <> col) <>)
        . mconcat $ numberedLinesToMarkup <$> reverse numbered
  where
    rows = fst (splitDiffByLines diff (0, 0) (before, after))
    numbered = foldl numberRows [] rows
    maxNumber = case numbered of
      [] -> 0
      ((x, _, y, _) : _) -> max x y

    digits :: Int -> Int
    digits n = let base = 10 :: Int in
      ceiling (logBase (fromIntegral base) (fromIntegral n) :: Double)

    columnWidth = max (20 + digits maxNumber * 8) 40

    numberedLinesToMarkup :: (Int, Line (SplitDiff a Info), Int, Line (SplitDiff a Info)) -> Markup
    numberedLinesToMarkup (m, left, n, right) = tr $ toMarkup (or $ hasChanges <$> left, m, renderable before left) <> toMarkup (or $ hasChanges <$> right, n, renderable after right) <> string "\n"

    renderable source = fmap (Renderable . (,) source)

    hasChanges diff = or $ const True <$> diff

    numberRows :: [(Int, Line a, Int, Line a)] -> Row a -> [(Int, Line a, Int, Line a)]
    numberRows [] (Row EmptyLine EmptyLine) = []
    numberRows [] (Row left@(Line _) EmptyLine) = [(1, left, 0, EmptyLine)]
    numberRows [] (Row EmptyLine right@(Line _)) = [(0, EmptyLine, 1, right)]
    numberRows [] (Row left right) = [(1, left, 1, right)]
    numberRows rows@((leftCount, _, rightCount, _):_) (Row EmptyLine EmptyLine) = (leftCount, EmptyLine, rightCount, EmptyLine):rows
    numberRows rows@((leftCount, _, rightCount, _):_) (Row left@(Line _) EmptyLine) = (leftCount + 1, left, rightCount, EmptyLine):rows
    numberRows rows@((leftCount, _, rightCount, _):_) (Row EmptyLine right@(Line _)) = (leftCount, EmptyLine, rightCount + 1, right):rows
    numberRows rows@((leftCount, _, rightCount, _):_) (Row left right) = (leftCount + 1, left, rightCount + 1, right):rows

-- | A diff with only one side’s annotations.
type SplitDiff leaf annotation = Free (Annotated leaf annotation) (Term leaf annotation)

newtype Renderable a = Renderable (Source Char, a)

instance ToMarkup f => ToMarkup (Renderable (Info, Syntax a (f, Range))) where
  toMarkup (Renderable (source, (Info range categories, syntax))) = classifyMarkup categories $ case syntax of
    Leaf _ -> span . string . toString $ slice range source
    Indexed children -> ul . mconcat $ contentElements children
    Fixed children -> ul . mconcat $ contentElements children
    Keyed children -> dl . mconcat $ contentElements children
    where markupForSeparatorAndChild :: ToMarkup f => ([Markup], Int) -> (f, Range) -> ([Markup], Int)
          markupForSeparatorAndChild (rows, previous) child = (rows ++ [ string  (toString $ slice (Range previous $ start $ snd child) source), toMarkup $ fst child ], end $ snd child)

          contentElements children = let (elements, previous) = foldl markupForSeparatorAndChild ([], start range) children in
            elements ++ [ string . toString $ slice (Range previous $ end range) source ]

instance ToMarkup (Renderable (Term a Info)) where
  toMarkup (Renderable (source, term)) = fst $ cata (\ info@(Info range _) syntax -> (toMarkup $ Renderable (source, (info, syntax)), range)) term

instance ToMarkup (Renderable (SplitDiff a Info)) where
  toMarkup (Renderable (source, diff)) = fst $ iter (\ (Annotated info@(Info range _) syntax) -> (toMarkup $ Renderable (source, (info, syntax)), range)) $ toMarkupAndRange <$> diff
    where toMarkupAndRange :: Term a Info -> (Markup, Range)
          toMarkupAndRange term@(Info range _ :< _) = ((div ! A.class_ (stringValue "patch")) . toMarkup $ Renderable (source, term), range)

splitDiffByLines :: Eq a => Diff a Info -> (Int, Int) -> (Source Char, Source Char) -> ([Row (SplitDiff a Info)], (Range, Range))
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
splitTermByLines :: Eq a => Term a Info -> Source Char -> ([Line (Term a Info)], Range)
splitTermByLines (Info range categories :< syntax) source = flip (,) range $ case syntax of
  Leaf a -> fmap (:< Leaf a) <$> contextLines range categories source
  Indexed children -> wrapLineContents (wrap Indexed) <$> adjoinChildLines Indexed children
  Fixed children -> adjoinChildLines Fixed children
  Keyed children -> adjoinChildLines Keyed children
  where adjoin = reverse . foldl (adjoinLinesBy $ openTerm source) []
        adjoinChildLines constructor children = let (lines, previous) = foldl (childLines $ constructor mempty) ([], start range) children in
          adjoin $ lines ++ (fmap (:< constructor mempty) <$> contextLines (Range previous $ end range) categories source)

        wrap constructor children = (Info (maybe mempty id $ foldl (<>) Nothing $ Just . getRange <$> children) categories :<) . constructor $ children

        getRange (Info range _ :< _) = range

        isContextBranch constructor (Info _ cc :< syntax) | constructor mempty == syntax, categories == cc = True
        isContextBranch _ _ = False

        childLines constructor (lines, previous) child = let (childLines, childRange) = splitTermByLines child source in
          (adjoin $ lines ++ (fmap (:< constructor) <$> contextLines (Range previous $ start childRange) categories source) ++ childLines, end childRange)

splitAnnotatedByLines :: Eq a => (Source Char, Source Char) -> (Range, Range) -> (Set.Set Category, Set.Set Category) -> Syntax a (Diff a Info) -> [Row (SplitDiff a Info)]
splitAnnotatedByLines sources ranges categories syntax = case syntax of
  Leaf a -> fmap (Free . (`Annotated` Leaf a)) <$> contextRows ranges categories sources
  Indexed children -> wrapRowContents (wrap Indexed (fst categories)) (wrap Indexed (snd categories)) <$> adjoinChildRows Indexed children
  Fixed children -> wrapRowContents (wrap Fixed (fst categories)) (wrap Fixed (snd categories)) <$> adjoinChildRows Fixed children
  Keyed children -> adjoinChildRows Keyed children
  where contextRows ranges categories sources = zipWithDefaults Row EmptyLine EmptyLine
          (contextLines (fst ranges) (fst categories) (fst sources))
          (contextLines (snd ranges) (snd categories) (snd sources))

        adjoin = reverse . foldl (adjoinRowsBy (openDiff $ fst sources) (openDiff $ snd sources)) []
        adjoinChildRows constructor children = let (rows, previous) = foldl (childRows constructor) ([], starts ranges) children in
          adjoin $ rows ++ (fmap (Free . (`Annotated` constructor mempty)) <$> contextRows (makeRanges previous (ends ranges)) categories sources)

        wrap constructor categories children = Free . Annotated (Info (maybe mempty id $ foldl (<>) Nothing $ Just . getRange <$> children) categories) . constructor $ filter (not . isContextBranch constructor categories) children

        getRange (Pure (Info range _ :< _)) = range
        getRange (Free (Annotated (Info range _) _)) = range

        isContextBranch constructor cc (Free (Annotated (Info _ categories) syntax)) | constructor mempty == syntax, categories == cc = True
        isContextBranch _ _ _ = False

        childRows constructor (rows, previous) child = let (childRows, childRanges) = splitDiffByLines child previous sources in
          (adjoin $ rows ++ (fmap (Free . (`Annotated` constructor mempty)) <$> contextRows (makeRanges previous (starts childRanges)) categories sources) ++ childRows, ends childRanges)

        starts (left, right) = (start left, start right)
        ends (left, right) = (end left, end right)
        makeRanges (leftStart, rightStart) (leftEnd, rightEnd) = (Range leftStart leftEnd, Range rightStart rightEnd)

contextLines :: Range -> Set.Set Category -> Source Char -> [Line Info]
contextLines range categories source = makeLine . (:[]) . (`Info` categories) <$> actualLineRanges range source

openRange :: Source Char -> Range -> Maybe Range
openRange source range = case (source `at`) <$> maybeLastIndex range of
  Just '\n' -> Nothing
  _ -> Just range

openTerm :: Source Char -> Term a Info -> Maybe (Term a Info)
openTerm source term@(Info range _ :< _) = const term <$> openRange source range

openDiff :: Source Char -> SplitDiff a Info -> Maybe (SplitDiff a Info)
openDiff source diff@(Free (Annotated (Info range _) _)) = const diff <$> openRange source range
openDiff source diff@(Pure term) = const diff <$> openTerm source term

zipWithDefaults :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithDefaults f da db a b = take (max (length a) (length b)) $ zipWith f (a ++ repeat da) (b ++ repeat db)
