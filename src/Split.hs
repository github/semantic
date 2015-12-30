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
import Text.Blaze.Html
import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text
import Data.Either
import Data.Functor.Identity
import Data.Monoid
import qualified OrderedMap as Map
import qualified Data.Set as Set
import Source hiding ((++))

type ClassName = T.Text

classifyMarkup :: Foldable f => f String -> Markup -> Markup
classifyMarkup categories element = maybe element ((element !) . A.class_ . stringValue . ("category-" ++)) $ maybeFirst categories

split :: Diff leaf Info -> Source Char -> Source Char -> IO TL.Text
split diff before after = return . renderHtml
  . docTypeHtml
    . ((head $ link ! A.rel "stylesheet" ! A.href "style.css") <>)
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

splitDiffByLines :: Diff leaf Info -> (Int, Int) -> (Source Char, Source Char) -> ([Row (SplitDiff leaf Info)], (Range, Range))
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

class Functor f => Has f where
  get :: f a -> a

instance Has Identity where
  get = runIdentity

instance Has ((,) a) where
  get = snd

-- | Takes a term and a source and returns a list of lines and their range within source.
splitTermByLines :: Term leaf Info -> Source Char -> ([Line (Term leaf Info)], Range)
splitTermByLines (Info range categories :< syntax) source = flip (,) range $ case syntax of
  Leaf a -> pure . (:< Leaf a) . (`Info` categories) <$> actualLineRanges range source
  Indexed children -> adjoinChildLines (Indexed . fmap get) (Identity <$> children)
  Fixed children -> adjoinChildLines (Fixed . fmap get) (Identity <$> children)
  Keyed children -> adjoinChildLines (Keyed . Map.fromList) (Map.toList children)
  where adjoin :: Has f => [Line (Either Range (f (Term leaf Info)))] -> [Line (Either Range (f (Term leaf Info)))]
        adjoin = reverse . foldl (adjoinLinesBy $ openEither (openRange source) (openTerm source)) []

        adjoinChildLines :: Has f => ([f (Term leaf Info)] -> Syntax leaf (Term leaf Info)) -> [f (Term leaf Info)] -> [Line (Term leaf Info)]
        adjoinChildLines constructor children = let (lines, previous) = foldl childLines ([], start range) children in
          fmap (wrapLineContents $ wrap constructor) . adjoin $ lines ++ (pure . Left <$> actualLineRanges (Range previous $ end range) source)

        wrap :: Has f => ([f (Term leaf Info)] -> Syntax leaf (Term leaf Info)) -> [Either Range (f (Term leaf Info))] -> Term leaf Info
        wrap constructor children = (Info (unionRanges $ getRange <$> children) categories :<) . constructor $ rights children

        getRange :: Has f => Either Range (f (Term leaf Info)) -> Range
        getRange (Right term) = case get term of (Info range _ :< _) -> range
        getRange (Left range) = range

        childLines :: Has f => ([Line (Either Range (f (Term leaf Info)))], Int) -> f (Term leaf Info) -> ([Line (Either Range (f (Term leaf Info)))], Int)
        childLines (lines, previous) child = let (childLines, childRange) = splitTermByLines (get child) source in
          (adjoin $ lines ++ (pure . Left <$> actualLineRanges (Range previous $ start childRange) source) ++ (fmap (Right . (<$ child)) <$> childLines), end childRange)

splitAnnotatedByLines :: (Source Char, Source Char) -> (Range, Range) -> (Set.Set Category, Set.Set Category) -> Syntax leaf (Diff leaf Info) -> [Row (SplitDiff leaf Info)]
splitAnnotatedByLines sources ranges categories syntax = case syntax of
  Leaf a -> wrapRowContents (Free . (`Annotated` Leaf a) . (`Info` fst categories) . unionRanges) (Free . (`Annotated` Leaf a) . (`Info` snd categories) . unionRanges) <$> contextRows ranges sources
  Indexed children -> adjoinChildRows (Indexed . fmap get) (Identity <$> children)
  Fixed children -> adjoinChildRows (Fixed . fmap get) (Identity <$> children)
  Keyed children -> adjoinChildRows (Keyed . Map.fromList) (Map.toList children)
  where contextRows :: (Range, Range) -> (Source Char, Source Char) -> [Row Range]
        contextRows ranges sources = zipWithDefaults Row EmptyLine EmptyLine
          (pure <$> actualLineRanges (fst ranges) (fst sources))
          (pure <$> actualLineRanges (snd ranges) (snd sources))

        adjoin :: Has f => [Row (Either Range (f (SplitDiff leaf Info)))] -> [Row (Either Range (f (SplitDiff leaf Info)))]
        adjoin = reverse . foldl (adjoinRowsBy (openEither (openRange $ fst sources) (openDiff $ fst sources)) (openEither (openRange $ snd sources) (openDiff $ snd sources))) []

        adjoinChildRows :: (Has f) => ([f (SplitDiff leaf Info)] -> Syntax leaf (SplitDiff leaf Info)) -> [f (Diff leaf Info)] -> [Row (SplitDiff leaf Info)]
        adjoinChildRows constructor children = let (rows, previous) = foldl childRows ([], starts ranges) children in
          fmap (wrapRowContents (wrap constructor (fst categories)) (wrap constructor (snd categories))) . adjoin $ rows ++ (fmap Left <$> contextRows (makeRanges previous (ends ranges)) sources)

        wrap :: Has f => ([f (SplitDiff leaf Info)] -> Syntax leaf (SplitDiff leaf Info)) -> Set.Set Category -> [Either Range (f (SplitDiff leaf Info))] -> SplitDiff leaf Info
        wrap constructor categories children = Free . Annotated (Info (unionRanges $ getRange <$> children) categories) . constructor $ rights children

        getRange :: Has f => Either Range (f (SplitDiff leaf Info)) -> Range
        getRange (Right diff) = case get diff of
          (Pure (Info range _ :< _)) -> range
          (Free (Annotated (Info range _) _)) -> range
        getRange (Left range) = range

        childRows :: (Has f) => ([Row (Either Range (f (SplitDiff leaf Info)))], (Int, Int)) -> f (Diff leaf Info) -> ([Row (Either Range (f (SplitDiff leaf Info)))], (Int, Int))
        childRows (rows, previous) child = let (childRows, childRanges) = splitDiffByLines (get child) previous sources in
          (adjoin $ rows ++ (fmap Left <$> contextRows (makeRanges previous (starts childRanges)) sources) ++ (fmap (Right . (<$ child)) <$> childRows), ends childRanges)

        starts (left, right) = (start left, start right)
        ends (left, right) = (end left, end right)
        makeRanges (leftStart, rightStart) (leftEnd, rightEnd) = (Range leftStart leftEnd, Range rightStart rightEnd)

openEither :: MaybeOpen a -> MaybeOpen b -> MaybeOpen (Either a b)
openEither ifLeft ifRight which = either (fmap (const which) . ifLeft) (fmap (const which) . ifRight) which

openRange :: Source Char -> MaybeOpen Range
openRange source range = case (source `at`) <$> maybeLastIndex range of
  Just '\n' -> Nothing
  _ -> Just range

openTerm :: Has f => Source Char -> MaybeOpen (f (Term leaf Info))
openTerm source term = const term <$> openRange source (case get term of (Info range _ :< _) -> range)

openDiff :: Has f => Source Char -> MaybeOpen (f (SplitDiff leaf Info))
openDiff source diff = const diff <$> case get diff of
  (Free (Annotated (Info range _) _)) -> openRange source range
  (Pure (Info range _ :< _)) -> openRange source range

zipWithDefaults :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithDefaults f da db a b = take (max (length a) (length b)) $ zipWith f (a ++ repeat da) (b ++ repeat db)
