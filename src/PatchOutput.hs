module PatchOutput (
  patch,
  hunks
) where

import Diff
import Line
import Row
import Source hiding ((++))
import Split
import Control.Comonad.Cofree
import Control.Monad.Free

patch :: Diff a Info -> Source Char -> Source Char -> String
patch diff sourceA sourceB = mconcat $ showHunk sourceA sourceB <$> hunks diff sourceA sourceB

data Hunk a = Hunk { offsetA :: Int, offsetB :: Int, changes :: [Change a] }
  deriving (Eq, Show)

data Change a = Change { context :: [Row a], contents :: [Row a] }
  deriving (Eq, Show)

showHunk :: Source Char -> Source Char -> Hunk (SplitDiff a Info) -> String
showHunk sourceA sourceB hunk = header hunk ++ concat (showChange sourceA sourceB <$> changes hunk)

showChange :: Source Char -> Source Char -> Change (SplitDiff a Info) -> String
showChange sourceA sourceB change = concat (showLine ' ' sourceB . unRight <$> context change) ++ concat (showRow <$> contents change)
  where showRow (Row lineA lineB) = showLine '-' sourceA lineA ++ showLine '+' sourceB lineB
        showLine _ _ EmptyLine = ""
        showLine prefix source line = prefix : (toString . (`slice` source) . mconcat $ getRange <$> unLine line)
        getRange (Free (Annotated (Info range _) _)) = range
        getRange (Pure (Info range _ :< _)) = range

header :: Hunk a -> String
header hunk = "@@ -" ++ show (offsetA hunk) ++ "," ++ show 0 ++ " +" ++ show (offsetB hunk) ++ "," ++ show 0 ++ " @@\n"

hunks :: Diff a Info -> Source Char -> Source Char -> [Hunk (SplitDiff a Info)]
hunks diff sourceA sourceB = hunksInRows rows
  where (rows, _) = splitDiffByLines diff (0, 0) (sourceA, sourceB)

hunksInRows :: [Row (SplitDiff a Info)] -> [Hunk (SplitDiff a Info)]
hunksInRows rows = case nextHunk rows of
  Nothing -> []
  Just (hunk, rest) -> hunk : hunksInRows rest

nextHunk :: [Row (SplitDiff a Info)] -> Maybe (Hunk (SplitDiff a Info), [Row (SplitDiff a Info)])
nextHunk rows = case nextChange rows of
  Nothing -> Nothing
  Just (change, rest) -> Just (Hunk 0 0 [ change ], rest)

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
