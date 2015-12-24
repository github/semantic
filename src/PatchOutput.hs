module PatchOutput (
  patch,
  hunks
) where

import Diff
import Row
import Source hiding ((++))
import Split

patch :: Diff a Info -> Source Char -> Source Char -> String
patch diff sourceA sourceB = mconcat $ show <$> hunks diff sourceA sourceB

data Hunk a = Hunk { offsetA :: Int, offsetB :: Int, getRows :: [Row (SplitDiff a Info)] }
  deriving Eq

instance Show (Hunk a) where
  show = header

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
nextHunk rows = Nothing
  where (leadingContext, afterLeadingContext) = Prelude.break rowHasChanges rows
        (changes, afterChanges) = span rowHasChanges afterLeadingContext

        rowHasChanges (Row left right) = lineHasChanges left || lineHasChanges right
        lineHasChanges = or . fmap diffHasChanges
        diffHasChanges = or . fmap (const True)
