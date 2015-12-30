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
import Control.Comonad.Cofree
import Control.Monad.Free

patch :: Eq a => Diff a Info -> Source Char -> Source Char -> String
patch diff sourceA sourceB = mconcat $ showHunk (sourceA, sourceB) <$> hunks diff (sourceA, sourceB)

data Hunk a = Hunk { offsetA :: Int, offsetB :: Int, changes :: [Change a], trailingContext :: [Row a] }
  deriving (Eq, Show)

data Change a = Change { context :: [Row a], contents :: [Row a] }
  deriving (Eq, Show)

showHunk :: Eq a => (Source Char, Source Char) -> Hunk (SplitDiff a Info) -> String
showHunk sources hunk = header hunk ++ concat (showChange sources <$> changes hunk) ++ concat (showLine ' ' (snd sources) . unRight <$> trailingContext hunk)

showChange :: Eq a => (Source Char, Source Char) -> Change (SplitDiff a Info) -> String
showChange sources change = concat (showLine ' ' (snd sources) . unRight <$> context change) ++ concat (showRow sources <$> contents change)

showRow :: Eq leaf => (Source Char, Source Char) -> Row (SplitDiff leaf Info) -> String
showRow sources (Row lineA lineB) = if lineA == lineB
  then showLine ' ' (snd sources) lineB
  else showLine '-' (fst sources) lineA ++ showLine '+' (snd sources) lineB

showLine :: Char -> Source Char -> Line (SplitDiff leaf Info) -> String
showLine _ _ EmptyLine = ""
showLine prefix source line = prefix : (toString . (`slice` source) . unionRanges $ getRange <$> unLine line)

getRange :: SplitDiff leaf Info -> Range
getRange (Free (Annotated (Info range _) _)) = range
getRange (Pure (Info range _ :< _)) = range

header :: Hunk a -> String
header hunk = "@@ -" ++ show (offsetA hunk) ++ "," ++ show (0 :: Int) ++ " +" ++ show (offsetB hunk) ++ "," ++ show (0 :: Int) ++ " @@\n"

hunks :: Diff a Info -> (Source Char, Source Char) -> [Hunk (SplitDiff a Info)]
hunks diff sources = hunksInRows . fst $ splitDiffByLines diff (0, 0) sources

hunksInRows :: [Row (SplitDiff a Info)] -> [Hunk (SplitDiff a Info)]
hunksInRows rows = case nextHunk rows of
  Nothing -> []
  Just (hunk, rest) -> hunk : hunksInRows rest

nextHunk :: [Row (SplitDiff a Info)] -> Maybe (Hunk (SplitDiff a Info), [Row (SplitDiff a Info)])
nextHunk rows = case nextChange rows of
  Nothing -> Nothing
  Just (change, rest) -> Just (Hunk 0 0 [ change ] (take 3 rest), drop 3 rest)

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
