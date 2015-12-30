module PatchOutput (
  patch,
  hunks
) where

import Diff
import Line
import Range
import Row
import Source hiding ((++))
import Split
import Control.Arrow
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Monoid

patch :: Diff a Info -> Source Char -> Source Char -> String
patch diff sourceA sourceB = mconcat $ showHunk (sourceA, sourceB) <$> hunks diff (sourceA, sourceB)

data Hunk a = Hunk { offset :: (Sum Int, Sum Int), changes :: [Change a], trailingContext :: [Row a] }
  deriving (Eq, Show)

data Change a = Change { context :: [Row a], contents :: [Row a] }
  deriving (Eq, Show)

hunkLength :: Hunk a -> (Int, Int)
hunkLength hunk = getSum *** getSum $ mconcat $ (changeLength <$> changes hunk) <> (rowLength <$> trailingContext hunk)

changeLength :: Change a -> (Sum Int, Sum Int)
changeLength change = mconcat $ (rowLength <$> context change) <> (rowLength <$> contents change)

rowLength :: Row a -> (Sum Int, Sum Int)
rowLength (Row a b) = (lineLength a, lineLength b)

lineLength :: Line a -> Sum Int
lineLength EmptyLine = 0
lineLength _ = 1

showHunk :: (Source Char, Source Char) -> Hunk (SplitDiff a Info) -> String
showHunk sources hunk = header hunk ++ concat (showChange sources <$> changes hunk) ++ concat (showRow sources <$> trailingContext hunk)

showChange :: (Source Char, Source Char) -> Change (SplitDiff a Info) -> String
showChange sources change = concat (showRow sources <$> context change) ++ concat (showRow sources <$> contents change)

showRow :: (Source Char, Source Char) -> Row (SplitDiff leaf Info) -> String
showRow sources (Row lineA lineB) = if stringA == stringB
  then maybe "" (' ' :) stringB
  else maybe "" ('-' :) stringA ++ maybe "" ('+' :) stringB
  where stringA = showLine (fst sources) lineA
        stringB = showLine (snd sources) lineB

showLine :: Source Char -> Line (SplitDiff leaf Info) -> Maybe String
showLine _ EmptyLine = Nothing
showLine source line = Just . toString . (`slice` source) . unionRanges $ getRange <$> unLine line

getRange :: SplitDiff leaf Info -> Range
getRange (Free (Annotated (Info range _) _)) = range
getRange (Pure (Info range _ :< _)) = range

header :: Hunk a -> String
header hunk = "@@ -" ++ show offsetA ++ "," ++ show lengthA ++ " +" ++ show offsetB ++ "," ++ show lengthB ++ " @@\n"
  where (lengthA, lengthB) = hunkLength hunk
        (offsetA, offsetB) = getSum *** getSum $ offset hunk

hunks :: Diff a Info -> (Source Char, Source Char) -> [Hunk (SplitDiff a Info)]
hunks diff sources = hunksInRows . fst $ splitDiffByLines diff (0, 0) sources

hunksInRows :: [Row (SplitDiff a Info)] -> [Hunk (SplitDiff a Info)]
hunksInRows rows = case nextHunk rows of
  Nothing -> []
  Just (hunk, rest) -> hunk : hunksInRows rest

nextHunk :: [Row (SplitDiff a Info)] -> Maybe (Hunk (SplitDiff a Info), [Row (SplitDiff a Info)])
nextHunk rows = case nextChange rows of
  Nothing -> Nothing
  Just (change, rest) -> Just (Hunk (0, 0) [ change ] (take 3 rest), drop 3 rest)

nextChange :: [Row (SplitDiff a Info)] -> Maybe (Change (SplitDiff a Info), [Row (SplitDiff a Info)])
nextChange rows = case changes of
  [] -> Nothing
  _ -> Just (Change (takeLast 3 leadingRows) changes, afterChanges)
  where (leadingRows, afterLeadingContext) = Prelude.break rowHasChanges rows
        (changes, afterChanges) = span rowHasChanges afterLeadingContext

rowHasChanges :: Row (SplitDiff a Info) -> Bool
rowHasChanges (Row left right) = lineHasChanges left || lineHasChanges right

lineHasChanges :: Line (SplitDiff a Info) -> Bool
lineHasChanges = or . fmap diffHasChanges

diffHasChanges :: SplitDiff a Info -> Bool
diffHasChanges = or . fmap (const True)

takeLast :: Int -> [a] -> [a]
takeLast n = fst . foldr accum ([], 0)
  where accum each (rest, i) = if i < n
          then (each : rest, i + 1)
          else (rest, i)
