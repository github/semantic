module PatchOutput (
  patch,
  hunks
) where

import Diff
import Line
import Range
import Renderer
import Row
import Source hiding ((++), break)
import Split
import Control.Arrow
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Maybe
import Data.Monoid

patch :: Renderer a String
patch diff (beforeBlob, afterBlob) = mconcat $ showHunk (beforeBlob, afterBlob) <$> hunks diff (before, after)
  where
    before = source beforeBlob
    after = source afterBlob

data Hunk a = Hunk { offset :: (Sum Int, Sum Int), changes :: [Change a], trailingContext :: [Row a] }
  deriving (Eq, Show)

data Change a = Change { context :: [Row a], contents :: [Row a] }
  deriving (Eq, Show)

hunkLength :: Hunk a -> (Sum Int, Sum Int)
hunkLength hunk = mconcat $ (changeLength <$> changes hunk) <> (rowLength <$> trailingContext hunk)

changeLength :: Change a -> (Sum Int, Sum Int)
changeLength change = mconcat $ (rowLength <$> context change) <> (rowLength <$> contents change)

rowLength :: Row a -> (Sum Int, Sum Int)
rowLength (Row a b) = (lineLength a, lineLength b)

lineLength :: Line a -> Sum Int
lineLength EmptyLine = 0
lineLength _ = 1

showHunk :: (SourceBlob, SourceBlob) -> Hunk (SplitDiff a Info) -> String
showHunk blobs@(beforeBlob, afterBlob) hunk = header blobs hunk ++ concat (showChange sources <$> changes hunk) ++ showLines (snd sources) ' ' (unRight <$> trailingContext hunk)
  where sources = (source beforeBlob, source afterBlob)

showChange :: (Source Char, Source Char) -> Change (SplitDiff a Info) -> String
showChange sources change = showLines (snd sources) ' ' (unRight <$> context change) ++ showLines (fst sources) '-' (unLeft <$> contents change) ++ showLines (snd sources) '+' (unRight <$> contents change)

showLines :: Source Char -> Char -> [Line (SplitDiff leaf Info)] -> String
showLines source prefix lines = fromMaybe "" . mconcat $ fmap (prefix :) . showLine source <$> lines

showLine :: Source Char -> Line (SplitDiff leaf Info) -> Maybe String
showLine _ EmptyLine = Nothing
showLine source line = Just . toString . (`slice` source) . unionRanges $ getRange <$> unLine line

getRange :: SplitDiff leaf Info -> Range
getRange (Free (Annotated (Info range _) _)) = range
getRange (Pure (Info range _ :< _)) = range

header :: (SourceBlob, SourceBlob) -> Hunk a -> String
header blobs hunk = "index " ++ oid (fst blobs) ++ " " ++ oid (snd blobs) ++ "\n" ++
  "@@ -" ++ show offsetA ++ "," ++ show lengthA ++ " +" ++ show offsetB ++ "," ++ show lengthB ++ " @@\n"
  where (lengthA, lengthB) = getSum *** getSum $ hunkLength hunk
        (offsetA, offsetB) = getSum *** getSum $ offset hunk

hunks :: Diff a Info -> (Source Char, Source Char) -> [Hunk (SplitDiff a Info)]
hunks diff sources = hunksInRows (1, 1) . fst $ splitDiffByLines diff (0, 0) sources

hunksInRows :: (Sum Int, Sum Int) -> [Row (SplitDiff a Info)] -> [Hunk (SplitDiff a Info)]
hunksInRows start rows = case nextHunk start rows of
  Nothing -> []
  Just (hunk, rest) -> hunk : hunksInRows (offset hunk <> hunkLength hunk) rest

nextHunk :: (Sum Int, Sum Int) -> [Row (SplitDiff a Info)] -> Maybe (Hunk (SplitDiff a Info), [Row (SplitDiff a Info)])
nextHunk start rows = case nextChange start rows of
  Nothing -> Nothing
  Just (offset, change, rest) -> let (changes, rest') = contiguousChanges rest in Just (Hunk offset (change : changes) $ take 3 rest', drop 3 rest')
  where contiguousChanges rows = case break rowHasChanges (take 7 rows) of
          (_, []) -> ([], rows)
          (context, _) -> case changeIncludingContext context (drop (length context) rows) of
            Nothing -> ([], rows)
            Just (change, rest) -> let (changes, rest') = contiguousChanges rest in (change : changes, rest')

nextChange :: (Sum Int, Sum Int) -> [Row (SplitDiff a Info)] -> Maybe ((Sum Int, Sum Int), Change (SplitDiff a Info), [Row (SplitDiff a Info)])
nextChange start rows = case changeIncludingContext leadingContext afterLeadingContext of
  Nothing -> Nothing
  Just (change, afterChanges) -> Just (start <> mconcat (rowLength <$> skippedContext), change, afterChanges)
  where (leadingRows, afterLeadingContext) = break rowHasChanges rows
        (skippedContext, leadingContext) = splitAt (max (length leadingRows - 3) 0) leadingRows

changeIncludingContext :: [Row (SplitDiff a Info)] -> [Row (SplitDiff a Info)] -> Maybe (Change (SplitDiff a Info), [Row (SplitDiff a Info)])
changeIncludingContext leadingContext rows = case changes of
  [] -> Nothing
  _ -> Just (Change leadingContext changes, afterChanges)
  where (changes, afterChanges) = span rowHasChanges rows

rowHasChanges :: Row (SplitDiff a Info) -> Bool
rowHasChanges (Row left right) = lineHasChanges left || lineHasChanges right

lineHasChanges :: Line (SplitDiff a Info) -> Bool
lineHasChanges = or . fmap diffHasChanges

diffHasChanges :: SplitDiff a Info -> Bool
diffHasChanges = or . fmap (const True)
