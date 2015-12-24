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

data Hunk a = Hunk { offsetA :: Int, offsetB :: Int, getRows :: [Row (SplitDiff a Info)] }
  deriving (Eq, Show)

showHunk sourceA sourceB hunk = header hunk ++ concat (showRow <$> getRows hunk)
  where showRow (Row lineA lineB) = showLine sourceA lineA ++ showLine sourceB lineB
        showLine _ EmptyLine = ""
        showLine source line = toString . (`slice` source) . mconcat $ getRange <$> unLine line
        getRange (Free (Annotated (Info range _) _)) = range
        getRange (Pure (Info range _ :< _)) = range

header :: Hunk a -> String
header hunk = "@@ -" ++ show (offsetA hunk) ++ "," ++ show 0 ++ " +" ++ show (offsetB hunk) ++ "," ++ show 0 ++ " @@\n"

hunks :: Diff a Info -> Source Char -> Source Char -> [Hunk a]
hunks diff sourceA sourceB = hunksInRows rows
  where (rows, _) = splitDiffByLines diff (0, 0) (sourceA, sourceB)

hunksInRows :: [Row (SplitDiff a Info)] -> [Hunk a]
hunksInRows rows = case nextHunk rows of
  Nothing -> []
  Just (hunk, rest) -> hunk : hunksInRows rest

nextHunk :: [Row (SplitDiff a Info)] -> Maybe (Hunk a, [Row (SplitDiff a Info)])
nextHunk rows = case hunkRows of
  [] -> Nothing
  hunkRows' -> Just (Hunk 0 0 hunkRows', afterChanges)
  where hunkRows = takeLast 3 leadingRows ++ changes

        (leadingRows, afterLeadingContext) = Prelude.break rowHasChanges rows
        (changes, afterChanges) = span rowHasChanges afterLeadingContext

        rowHasChanges (Row left right) = lineHasChanges left || lineHasChanges right
        lineHasChanges = or . fmap diffHasChanges
        diffHasChanges = or . fmap (const True)

takeLast :: Int -> [a] -> [a]
takeLast n = fst . foldr accum ([], 0)
  where accum each (rest, i) = if i < n
          then (each : rest, i + 1)
          else (rest, i)
