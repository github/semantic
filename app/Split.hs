module Split (split) where

import Diff
import Patch
import Syntax
import Term
import Range
import Control.Monad.Free
import qualified Data.Map as Map
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

data Row = Row (Maybe HTML) (Maybe HTML)

diffToRows :: Diff a Info -> String -> String -> [Row]
diffToRows (Free (Annotated (left, right) syntax)) = annotationAndSyntaxToRows (left, right) syntax

annotationAndSyntaxToRows :: (Info, Info) -> Syntax a (Diff a Info) -> String -> String -> [Row]
annotationAndSyntaxToRows (Info left _ leftCategories, Info right _ rightCategories) (Leaf _) before after = [] where
  leftElements = Span (classify leftCategories) <$> lines (substring left before)
  rightElements = Span (classify rightCategories) <$> lines (substring right after)

zipMaybe :: [a] -> [b] -> [(Maybe a, Maybe b)]
zipMaybe la lb = take len $ zip la' lb'
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

splitHTMLIntoLines :: HTML -> [HTML]
splitHTMLIntoLines (Text string) = Text <$> lines string
splitHTMLIntoLines (Span className string) = Span className <$> lines string
splitHTMLIntoLines (Ul className children) = Ul className <$> foldr combineLines [[]] children
splitHTMLIntoLines (Dl className children) = Dl className <$> foldr combineLines [[]] children
splitHTMLIntoLines (Dt string) = [ Dt string ]

combineLines :: HTML -> [[HTML]] -> [[HTML]]
combineLines child out = case splitHTMLIntoLines child of
  (first : rest) -> appendOntoLastLine first out ++ ((: []) <$> rest)

appendOntoLastLine :: HTML -> [[HTML]] -> [[HTML]]
appendOntoLastLine line [ x ] = [ line : x ]
appendOntoLastLine line (x : xs) = x : appendOntoLastLine line xs

classify :: Set.Set Category -> Maybe ClassName
classify = foldr (const . Just) Nothing
