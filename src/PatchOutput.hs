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
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Maybe
import Data.Monoid
import Data.Bifunctor
import Control.Monad

-- | Render a diff in the traditional patch format.
patch :: Renderer a String
patch diff sources = mconcat $ showHunk sources <$> hunks diff sources

-- | A hunk in a patch, including the offset, changes, and context.
data Hunk a = Hunk { offset :: (Sum Int, Sum Int), changes :: [Change a], trailingContext :: [Row a] }
  deriving (Eq, Show)

-- | A change in a patch hunk, along with its preceding context.
data Change a = Change { context :: [Row a], contents :: [Row a] }
  deriving (Eq, Show)

-- | The number of lines in the hunk before and after.
hunkLength :: Hunk a -> (Sum Int, Sum Int)
hunkLength hunk = mconcat $ (changeLength <$> changes hunk) <> (rowLength <$> trailingContext hunk)

-- | The number of lines in change before and after.
changeLength :: Change a -> (Sum Int, Sum Int)
changeLength change = mconcat $ (rowLength <$> context change) <> (rowLength <$> contents change)

-- | The number of lines in the row, each being either 0 or 1.
rowLength :: Row a -> (Sum Int, Sum Int)
rowLength (Row a b) = (lineLength a, lineLength b)

-- | The length of the line, being either 0 or 1.
lineLength :: Line a -> Sum Int
lineLength EmptyLine = 0
lineLength _ = 1

showHunk :: (SourceBlob, SourceBlob) -> Hunk (SplitDiff a Info) -> String
showHunk blobs@(beforeBlob, afterBlob) hunk = header blobs hunk ++ concat (showChange sources <$> changes hunk) ++ showLines (snd sources) ' ' (unRight <$> trailingContext hunk)
  where sources = (source beforeBlob, source afterBlob)

-- | Given the before and after sources, render a change to a string.
showChange :: (Source Char, Source Char) -> Change (SplitDiff a Info) -> String
showChange sources change = showLines (snd sources) ' ' (unRight <$> context change) ++ showLines (fst sources) '-' (unLeft <$> contents change) ++ showLines (snd sources) '+' (unRight <$> contents change)

-- | Given a source, render a set of lines to a string with a prefix.
showLines :: Source Char -> Char -> [Line (SplitDiff leaf Info)] -> String
showLines source prefix lines = fromMaybe "" . mconcat $ fmap (prefix :) . showLine source <$> lines

-- | Given a source, render a line to a string.
showLine :: Source Char -> Line (SplitDiff leaf Info) -> Maybe String
showLine _ EmptyLine = Nothing
showLine source line = Just . toString . (`slice` source) . unionRanges $ getRange <$> unLine line

-- | Return the range from a split diff.
getRange :: SplitDiff leaf Info -> Range
getRange (Free (Annotated (Info range _) _)) = range
getRange (Pure (Info range _ :< _)) = range

header :: (SourceBlob, SourceBlob) -> Hunk a -> String
header blobs hunk = "diff --git a/" ++ path (fst blobs) ++ " b/" ++ path (snd blobs) ++ "\n" ++
  "index " ++ oid (fst blobs) ++ ".." ++ oid (snd blobs) ++ "\n" ++
  "@@ -" ++ show offsetA ++ "," ++ show lengthA ++ " +" ++ show offsetB ++ "," ++ show lengthB ++ " @@\n"
  where (lengthA, lengthB) = join bimap getSum $ hunkLength hunk
        (offsetA, offsetB) = join bimap getSum $ offset hunk

-- | Render a diff as a series of hunks.
hunks :: Renderer a [Hunk (SplitDiff a Info)]
hunks diff (beforeBlob, afterBlob) = hunksInRows (1, 1) . fst $ splitDiffByLines diff (0, 0) (before, after)
  where
    before = source beforeBlob
    after = source afterBlob
-- | Given beginning line numbers, turn rows in a split diff into hunks in a
-- | patch.
hunksInRows :: (Sum Int, Sum Int) -> [Row (SplitDiff a Info)] -> [Hunk (SplitDiff a Info)]
hunksInRows start rows = case nextHunk start rows of
  Nothing -> []
  Just (hunk, rest) -> hunk : hunksInRows (offset hunk <> hunkLength hunk) rest

-- | Given beginning line numbers, return the next hunk and the remaining rows
-- | of the split diff.
nextHunk :: (Sum Int, Sum Int) -> [Row (SplitDiff a Info)] -> Maybe (Hunk (SplitDiff a Info), [Row (SplitDiff a Info)])
nextHunk start rows = case nextChange start rows of
  Nothing -> Nothing
  Just (offset, change, rest) -> let (changes, rest') = contiguousChanges rest in Just (Hunk offset (change : changes) $ take 3 rest', drop 3 rest')
  where contiguousChanges rows = case break rowHasChanges (take 7 rows) of
          (_, []) -> ([], rows)
          (context, _) -> case changeIncludingContext context (drop (length context) rows) of
            Nothing -> ([], rows)
            Just (change, rest) -> let (changes, rest') = contiguousChanges rest in (change : changes, rest')

-- | Given beginning line numbers, return the number of lines to the next
-- | the next change, and the remaining rows of the split diff.
nextChange :: (Sum Int, Sum Int) -> [Row (SplitDiff a Info)] -> Maybe ((Sum Int, Sum Int), Change (SplitDiff a Info), [Row (SplitDiff a Info)])
nextChange start rows = case changeIncludingContext leadingContext afterLeadingContext of
  Nothing -> Nothing
  Just (change, afterChanges) -> Just (start <> mconcat (rowLength <$> skippedContext), change, afterChanges)
  where (leadingRows, afterLeadingContext) = break rowHasChanges rows
        (skippedContext, leadingContext) = splitAt (max (length leadingRows - 3) 0) leadingRows

-- | Return a Change with the given context and the rows from the begginning of
-- | the given rows that have changes, or Nothing if the first row has no
-- | changes.
changeIncludingContext :: [Row (SplitDiff a Info)] -> [Row (SplitDiff a Info)] -> Maybe (Change (SplitDiff a Info), [Row (SplitDiff a Info)])
changeIncludingContext leadingContext rows = case changes of
  [] -> Nothing
  _ -> Just (Change leadingContext changes, afterChanges)
  where (changes, afterChanges) = span rowHasChanges rows

-- | Whether a row has changes on either side.
rowHasChanges :: Row (SplitDiff a Info) -> Bool
rowHasChanges (Row left right) = lineHasChanges left || lineHasChanges right

-- | Whether a line has changes.
lineHasChanges :: Line (SplitDiff a Info) -> Bool
lineHasChanges = or . fmap diffHasChanges

-- | Whether a split diff has changes.
diffHasChanges :: SplitDiff a Info -> Bool
diffHasChanges = or . fmap (const True)
