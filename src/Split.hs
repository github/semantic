module Split where

import Diff
import Patch
import Syntax
import Term
import Range
import Control.Monad.Free
import qualified Data.Map as Map
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
    sumRows (rows, previousIndices) child = (rows `adjoinRows` (contextRows (starts childRanges) previousIndices sources) `adjoinRows` childRows
                                            , ends childRanges)
      where
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
adjoinRows accum (row : rows) = init accum ++ [ last accum <> row ] ++ rows

zipWithMaybe :: (Maybe a -> Maybe b -> c) -> [a] -> [b] -> [c]
zipWithMaybe f la lb = take len $ zipWith f la' lb'
  where
    len = max (length la) (length lb)
    la' = (Just <$> la) ++ (repeat Nothing)
    lb' = (Just <$> lb) ++ (repeat Nothing)

splitDiff :: Diff a Info -> String -> String -> Patch (HTML, Range)
splitDiff diff before after = iter toElements $ splitPatch before after <$> diff
  where
    toElements (Annotated (left, right) (Leaf _)) = Replace (leafToElement before left) (leafToElement after right)

    leafToElement source (Info range _ categories) = (Span (classify categories) $ substring range source, range)

splitPatch :: String -> String -> Patch (Term a Info) -> Patch (HTML, Range)
splitPatch before after (Replace a b) = Replace (termToHTML before a) (termToHTML after b)
splitPatch _ after (Insert b) = Insert $ termToHTML after b
splitPatch before _ (Delete a) = Delete $ termToHTML before a

termToHTML :: String -> Term a Info -> (HTML, Range)
termToHTML source = cata toElement where
  toElement (Info range _ categories) (Leaf _) = (Span (classify categories) $ substring range source, range)
  toElement (Info range _ categories) (Indexed i) = makeList i range categories
  toElement (Info range _ categories) (Fixed i) = makeList i range categories
  toElement (Info range _ categories) (Keyed k) = makeMap (Map.toList k) range categories

  accumulate (children, previous) (child, range) = (children ++ [ subtext previous $ start range, child ], end range)
  accumulateFromMap (children, previous) (key, (child, range)) = (children ++ [ subtext previous $ start range, Dt key, child ], end range)

  makeList i range categories = (Ul (classify categories) items, range)
    where
      (children, previous) = foldl accumulate ([], start range) i
      items = children ++ [ subtext previous $ end range ]

  makeMap k range categories = (Dl (classify categories) items, range)
    where
      (children, previous) = foldl accumulateFromMap ([], start range) k
      items = children ++ [ subtext previous $ end range ]

  subtext :: Int -> Int -> HTML
  subtext start end = Text $ substring (Range start end) source

classify :: Set.Set Category -> Maybe ClassName
classify = foldr (const . Just) Nothing

actualLines :: String -> [String]
actualLines "" = [""]
actualLines lines = case break (== '\n') lines of
  (l, lines') -> l : (case lines' of
                       [] -> []
                       _:lines' -> actualLines lines')
