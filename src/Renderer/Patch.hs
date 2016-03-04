module Renderer.Patch (
  patch,
  hunks,
  Hunk(..)
) where

import Alignment
import Diff
import Line
import Prelude hiding (fst, snd)
import qualified Prelude
import Range
import Renderer
import Row
import Source hiding ((++), break)
import SplitDiff
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Functor.Both
import Data.Maybe
import Data.Monoid

-- | Render a diff in the traditional patch format.
patch :: Renderer a String
patch diff sources = mconcat $ showHunk sources <$> hunks diff sources

-- | A hunk in a patch, including the offset, changes, and context.
data Hunk a = Hunk { offset :: Both (Sum Int), changes :: [Change a], trailingContext :: [Row a] }
  deriving (Eq, Show)

-- | A change in a patch hunk, along with its preceding context.
data Change a = Change { context :: [Row a], contents :: [Row a] }
  deriving (Eq, Show)

-- | The number of lines in the hunk before and after.
hunkLength :: Hunk a -> Both (Sum Int)
hunkLength hunk = mconcat $ (changeLength <$> changes hunk) <> (rowLength <$> trailingContext hunk)

-- | The number of lines in change before and after.
changeLength :: Change a -> Both (Sum Int)
changeLength change = mconcat $ (rowLength <$> context change) <> (rowLength <$> contents change)

-- | The number of lines in the row, each being either 0 or 1.
rowLength :: Row a -> Both (Sum Int)
rowLength = fmap lineLength . unRow

-- | The length of the line, being either 0 or 1.
lineLength :: Line a -> Sum Int
lineLength EmptyLine = 0
lineLength _ = 1

-- | Given the before and after sources, render a hunk to a string.
showHunk :: Both SourceBlob -> Hunk (SplitDiff a Info) -> String
showHunk blobs hunk = header blobs hunk ++ concat (showChange sources <$> changes hunk) ++ showLines (snd sources) ' ' (unRight <$> trailingContext hunk)
  where sources = source <$> blobs

-- | Given the before and after sources, render a change to a string.
showChange :: Both (Source Char) -> Change (SplitDiff a Info) -> String
showChange sources change = showLines (snd sources) ' ' (unRight <$> context change) ++ deleted ++ inserted
  where (deleted, inserted) = runBoth $ pure showLines <*> sources <*> Both ('-', '+') <*> (pure fmap <*> Both (unLeft, unRight) <*> pure (contents change))

-- | Given a source, render a set of lines to a string with a prefix.
showLines :: Source Char -> Char -> [Line (SplitDiff leaf Info)] -> String
showLines source prefix lines = fromMaybe "" . mconcat $ fmap prepend . showLine source <$> lines
  where prepend "" = ""
        prepend source = prefix : source

-- | Given a source, render a line to a string.
showLine :: Source Char -> Line (SplitDiff leaf Info) -> Maybe String
showLine _ EmptyLine = Nothing
showLine source line = Just . toString . (`slice` source) . unionRanges $ getRange <$> unLine line

-- | Return the range from a split diff.
getRange :: SplitDiff leaf Info -> Range
getRange (Free (Annotated (Info range _) _)) = range
getRange (Pure patch) = let Info range _ :< _ = getSplitTerm patch in range

-- | Returns the header given two source blobs and a hunk.
header :: Both SourceBlob -> Hunk a -> String
header blobs hunk = intercalate "\n" [filepathHeader, fileModeHeader, beforeFilepath, afterFilepath, maybeOffsetHeader]
  where filepathHeader = "diff --git a/" ++ pathA ++ " b/" ++ pathB
        fileModeHeader = case (modeA, modeB) of
          (Nothing, Just mode) -> intercalate "\n" [ "new file mode " ++ modeToDigits mode, blobOidHeader ]
          (Just mode, Nothing) -> intercalate "\n" [ "old file mode " ++ modeToDigits mode, blobOidHeader ]
          (Just mode, Just other) | mode == other -> "index " ++ oidA ++ ".." ++ oidB ++ " " ++ modeToDigits mode
          (Just mode1, Just mode2) -> intercalate "\n" [
            "old mode" ++ modeToDigits mode1,
            "new mode " ++ modeToDigits mode2 ++ " " ++ blobOidHeader
            ]
        blobOidHeader = "index " ++ oidA ++ ".." ++ oidB
        modeHeader :: String -> Maybe SourceKind -> String -> String
        modeHeader ty maybeMode path = case maybeMode of
           Just mode -> ty ++ "a" ++ path
           Nothing -> "/dev/null"
        beforeFilepath = "--- " ++ modeHeader "a" modeA pathA
        afterFilepath = "+++ " ++ modeHeader "b" modeB pathB
        maybeOffsetHeader = if lengthA > 0 && lengthB > 0
                            then offsetHeader
                            else mempty
        offsetHeader = intercalate "," ["@@ -", show offsetA, show lengthA, show offsetB, show lengthB, " @@"] ++ "\n"
        (lengthA, lengthB) = runBoth . fmap getSum $ hunkLength hunk
        (offsetA, offsetB) = runBoth . fmap getSum $ offset hunk
        (pathA, pathB) = runBoth $ path <$> blobs
        (oidA, oidB) = runBoth $ oid <$> blobs
        (modeA, modeB) = runBoth $ blobKind <$> blobs

-- | Render a diff as a series of hunks.
hunks :: Renderer a [Hunk (SplitDiff a Info)]
hunks _ blobs | Both (True, True) <- Source.null . source <$> blobs = [Hunk { offset = mempty, changes = [], trailingContext = [] }]
hunks diff blobs = hunksInRows (Both (1, 1)) . Prelude.fst $ splitDiffByLines diff (pure 0) (source <$> blobs)

-- | Given beginning line numbers, turn rows in a split diff into hunks in a
-- | patch.
hunksInRows :: Both (Sum Int) -> [Row (SplitDiff a Info)] -> [Hunk (SplitDiff a Info)]
hunksInRows start rows = case nextHunk start rows of
  Nothing -> []
  Just (hunk, rest) -> hunk : hunksInRows (offset hunk <> hunkLength hunk) rest

-- | Given beginning line numbers, return the next hunk and the remaining rows
-- | of the split diff.
nextHunk :: Both (Sum Int) -> [Row (SplitDiff a Info)] -> Maybe (Hunk (SplitDiff a Info), [Row (SplitDiff a Info)])
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
nextChange :: Both (Sum Int) -> [Row (SplitDiff a Info)] -> Maybe (Both (Sum Int), Change (SplitDiff a Info), [Row (SplitDiff a Info)])
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
rowHasChanges (Row lines) = or (lineHasChanges <$> lines)

-- | Whether a line has changes.
lineHasChanges :: Line (SplitDiff a Info) -> Bool
lineHasChanges = or . fmap diffHasChanges

-- | Whether a split diff has changes.
diffHasChanges :: SplitDiff a Info -> Bool
diffHasChanges = or . fmap (const True)
