module Alignment where

import Category
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Either
import Data.Foldable (foldl')
import Data.Functor.Both
import Data.Functor.Identity
import qualified Data.List as List
import qualified Data.OrderedMap as Map
import qualified Data.Set as Set
import Diff
import Line
import Patch
import Prelude hiding (fst, snd)
import qualified Prelude
import Range
import Row
import Source hiding ((++))
import SplitDiff
import Syntax
import Term

-- | Assign line numbers to the lines on each side of a list of rows.
numberedRows :: [Row a] -> [Both (Int, Line a)]
numberedRows = foldl' numberRows []
  where numberRows rows row = ((,) <$> ((+) <$> count rows <*> (valueOf <$> unRow row)) <*> unRow row) : rows
        count = maybe (pure 0) (fmap Prelude.fst) . maybeFirst
        valueOf EmptyLine = 0
        valueOf _ = 1

-- | Determine whether a line contains any patches.
hasChanges :: Line (SplitDiff leaf Info) -> Bool
hasChanges = or . fmap (or . (True <$))

-- | Split a diff, which may span multiple lines, into rows of split diffs.
splitDiffByLines :: Diff leaf Info -> Both Int -> Both (Source Char) -> ([Row (SplitDiff leaf Info)], Both Range)
splitDiffByLines diff previous sources = case diff of
  Free (Annotated annotation syntax) -> (splitAnnotatedByLines sources (ranges annotation) (Diff.categories <$> annotation) syntax, ranges annotation)
  Pure patch -> splitPatchByLines patch previous sources
  where ranges annotations = characterRange <$> annotations

-- | Split a patch, which may span multiple lines, into rows of split diffs.
splitPatchByLines :: Patch (Term leaf Info) -> Both Int -> Both (Source Char) -> ([Row (SplitDiff leaf Info)], Both Range)
splitPatchByLines patch previous sources = case patch of
  Insert term -> let (lines, range) = splitTermByLines term (snd sources) in
    (makeRow EmptyLine . fmap (Pure . SplitInsert) <$> lines, both (rangeAt $ fst previous) range)
  Delete term -> let (lines, range) = splitTermByLines term (fst sources) in
    (flip makeRow EmptyLine . fmap (Pure . SplitDelete) <$> lines, both range (rangeAt $ snd previous))
  Replace leftTerm rightTerm -> (zipWithDefaults makeRow (pure mempty) $ fmap (fmap (Pure . SplitReplace)) <$> lines, ranges)
    where (lines, ranges) = transpose $ splitTermByLines <$> both leftTerm rightTerm <*> sources

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

-- | Split a annotated diff into rows of split diffs.
splitAnnotatedByLines :: Both (Source Char) -> Both Range -> Both (Set.Set Category) -> Syntax leaf (Diff leaf Info) -> [Row (SplitDiff leaf Info)]
splitAnnotatedByLines sources ranges categories syntax = case syntax of
  Leaf a -> wrapRowContents (((Free . (`Annotated` Leaf a)) .) <$> ((. unionRanges) . flip Info <$> categories)) <$> contextRows ranges sources
  Indexed children -> adjoinChildRows (Indexed . fmap get) (Identity <$> children)
  Fixed children -> adjoinChildRows (Fixed . fmap get) (Identity <$> children)
  Keyed children -> adjoinChildRows (Keyed . Map.fromList) (List.sortOn (diffRanges . Prelude.snd) $ Map.toList children)
  where contextRows :: Both Range -> Both (Source Char) -> [Row Range]
        contextRows ranges sources = zipWithDefaults makeRow (pure mempty) (fmap pure <$> (actualLineRanges <$> ranges <*> sources))

        adjoin :: Has f => [Row (Either Range (f (SplitDiff leaf Info)))] -> [Row (Either Range (f (SplitDiff leaf Info)))]
        adjoin = reverse . foldl (adjoinRowsBy (openEither <$> (openRange <$> sources) <*> (openDiff <$> sources))) []

        adjoinChildRows :: Has f => ([f (SplitDiff leaf Info)] -> Syntax leaf (SplitDiff leaf Info)) -> [f (Diff leaf Info)] -> [Row (SplitDiff leaf Info)]
        adjoinChildRows constructor children = let (rows, previous) = foldl childRows ([], start <$> ranges) children in
          fmap (wrapRowContents (wrap constructor <$> categories)) . adjoin $ rows ++ (fmap Left <$> contextRows (makeRanges previous (end <$> ranges)) sources)

        wrap :: Has f => ([f (SplitDiff leaf Info)] -> Syntax leaf (SplitDiff leaf Info)) -> Set.Set Category -> [Either Range (f (SplitDiff leaf Info))] -> SplitDiff leaf Info
        wrap constructor categories children = Free . Annotated (Info (unionRanges $ getRange <$> children) categories) . constructor $ rights children

        getRange :: Has f => Either Range (f (SplitDiff leaf Info)) -> Range
        getRange (Right diff) = case get diff of
          (Pure patch) -> let Info range _ :< _ = getSplitTerm patch in range
          (Free (Annotated (Info range _) _)) -> range
        getRange (Left range) = range

        childRows :: Has f => ([Row (Either Range (f (SplitDiff leaf Info)))], Both Int) -> f (Diff leaf Info) -> ([Row (Either Range (f (SplitDiff leaf Info)))], Both Int)
        childRows (rows, previous) child = let (childRows, childRanges) = splitDiffByLines (get child) previous sources in
          (adjoin $ rows ++ (fmap Left <$> contextRows (makeRanges previous (start <$> childRanges)) sources) ++ (fmap (Right . (<$ child)) <$> childRows), end <$> childRanges)

        makeRanges :: Both Int -> Both Int -> Both Range
        makeRanges a b = runBothWith Range <$> sequenceA (both a b)

-- | Produces the starting indices of a diff.
diffRanges :: Diff leaf Info -> Both (Maybe Range)
diffRanges (Free (Annotated infos _)) = Just . characterRange <$> infos
diffRanges (Pure patch) = fmap (characterRange . extract) <$> unPatch patch

-- | Returns a function that takes an Either, applies either the left or right
-- | MaybeOpen, and returns Nothing or the original either.
openEither :: MaybeOpen a -> MaybeOpen b -> MaybeOpen (Either a b)
openEither ifLeft ifRight which = either (fmap (const which) . ifLeft) (fmap (const which) . ifRight) which

-- | Given a source and a range, returns nothing if it ends with a `\n`;
-- | otherwise returns the range.
openRange :: Source Char -> MaybeOpen Range
openRange source range = case (source `at`) <$> maybeLastIndex range of
  Just '\n' -> Nothing
  _ -> Just range

-- | Given a source and something that has a term, returns nothing if the term
-- | ends with a `\n`; otherwise returns the term.
openTerm :: Has f => Source Char -> MaybeOpen (f (Term leaf Info))
openTerm source term = const term <$> openRange source (case get term of (Info range _ :< _) -> range)

-- | Given a source and something that has a split diff, returns nothing if the
-- | diff ends with a `\n`; otherwise returns the diff.
openDiff :: Has f => Source Char -> MaybeOpen (f (SplitDiff leaf Info))
openDiff source diff = const diff <$> case get diff of
  (Free (Annotated (Info range _) _)) -> openRange source range
  (Pure patch) -> let Info range _ :< _ = getSplitTerm patch in openRange source range

-- | A functor that can return its content.
class Functor f => Has f where
  get :: f a -> a

instance Has Identity where
  get = runIdentity

instance Has ((,) a) where
  get = Prelude.snd
