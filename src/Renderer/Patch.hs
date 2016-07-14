module Renderer.Patch (
  patch,
  hunks,
  Hunk(..),
  truncatePatch
) where

import Alignment
import Data.Bifunctor.Join
import Data.Functor.Both as Both
import Data.List (span, unzip)
import Data.Record
import Data.String
import Data.Text (pack)
import Data.These
import Diff
import Info
import Patch
import Prologue hiding (fst, snd)
import Range
import Renderer
import Source hiding ((<>), break)
import SplitDiff

-- | Render a timed out file as a truncated diff.
truncatePatch :: DiffArguments -> Both SourceBlob -> Text
truncatePatch _ blobs = pack $ header blobs <> "#timed_out\nTruncating diff: timeout reached.\n"

-- | Render a diff in the traditional patch format.
patch :: Renderer
patch diff blobs = pack $ case getLast (foldMap (Last . Just) string) of
  Just c | c /= '\n' -> string <> "\n\\ No newline at end of file\n"
  _ -> string
  where string = header blobs <> mconcat (showHunk blobs <$> hunks diff blobs)

-- | A hunk in a patch, including the offset, changes, and context.
data Hunk a = Hunk { offset :: Both (Sum Int), changes :: [Change a], trailingContext :: [Join These a] }
  deriving (Eq, Show)

-- | A change in a patch hunk, along with its preceding context.
data Change a = Change { context :: [Join These a], contents :: [Join These a] }
  deriving (Eq, Show)

-- | The number of lines in the hunk before and after.
hunkLength :: Hunk a -> Both (Sum Int)
hunkLength hunk = mconcat $ (changeLength <$> changes hunk) <> (rowIncrement <$> trailingContext hunk)

-- | The number of lines in change before and after.
changeLength :: Change a -> Both (Sum Int)
changeLength change = mconcat $ (rowIncrement <$> context change) <> (rowIncrement <$> contents change)

-- | The increment the given row implies for line numbering.
rowIncrement :: Join These a -> Both (Sum Int)
rowIncrement = Join . fromThese (Sum 0) (Sum 0) . runJoin . (Sum 1 <$)

-- | Given the before and after sources, render a hunk to a string.
showHunk :: HasField fields Range => Both SourceBlob -> Hunk (SplitDiff a (Record fields)) -> String
showHunk blobs hunk = maybeOffsetHeader <>
  concat (showChange sources <$> changes hunk) <>
  showLines (snd sources) ' ' (maybeSnd . runJoin <$> trailingContext hunk)
  where sources = source <$> blobs
        maybeOffsetHeader = if lengthA > 0 && lengthB > 0
                            then offsetHeader
                            else mempty
        offsetHeader = "@@ -" <> offsetA <> "," <> show lengthA <> " +" <> offsetB <> "," <> show lengthB <> " @@" <> "\n"
        (lengthA, lengthB) = runJoin . fmap getSum $ hunkLength hunk
        (offsetA, offsetB) = runJoin . fmap (show . getSum) $ offset hunk

-- | Given the before and after sources, render a change to a string.
showChange :: HasField fields Range => Both (Source Char) -> Change (SplitDiff a (Record fields)) -> String
showChange sources change = showLines (snd sources) ' ' (maybeSnd . runJoin <$> context change) <> deleted <> inserted
  where (deleted, inserted) = runJoin $ pure showLines <*> sources <*> both '-' '+' <*> Join (unzip (fromThese Nothing Nothing . runJoin . fmap Just <$> contents change))

-- | Given a source, render a set of lines to a string with a prefix.
showLines :: HasField fields Range => Source Char -> Char -> [Maybe (SplitDiff leaf (Record fields))] -> String
showLines source prefix lines = fromMaybe "" . mconcat $ fmap prepend . showLine source <$> lines
  where prepend "" = ""
        prepend source = prefix : source

-- | Given a source, render a line to a string.
showLine :: HasField fields Range => Source Char -> Maybe (SplitDiff leaf (Record fields)) -> Maybe String
showLine source line | Just line <- line = Just . toString . (`slice` source) $ getRange line
                     | otherwise = Nothing

-- | Returns the header given two source blobs and a hunk.
header :: Both SourceBlob -> String
header blobs = intercalate "\n" [filepathHeader, fileModeHeader, beforeFilepath, afterFilepath] <> "\n"
  where filepathHeader = "diff --git a/" <> pathA <> " b/" <> pathB
        fileModeHeader = case (modeA, modeB) of
          (Nothing, Just mode) -> intercalate "\n" [ "new file mode " <> modeToDigits mode, blobOidHeader ]
          (Just mode, Nothing) -> intercalate "\n" [ "deleted file mode " <> modeToDigits mode, blobOidHeader ]
          (Just mode, Just other) | mode == other -> "index " <> oidA <> ".." <> oidB <> " " <> modeToDigits mode
          (Just mode1, Just mode2) -> intercalate "\n" [
            "old mode " <> modeToDigits mode1,
            "new mode " <> modeToDigits mode2,
            blobOidHeader
            ]
          (Nothing, Nothing) -> ""
        blobOidHeader = "index " <> oidA <> ".." <> oidB
        modeHeader :: String -> Maybe SourceKind -> String -> String
        modeHeader ty maybeMode path = case maybeMode of
           Just _ -> ty <> "/" <> path
           Nothing -> "/dev/null"
        beforeFilepath = "--- " <> modeHeader "a" modeA pathA
        afterFilepath = "+++ " <> modeHeader "b" modeB pathB
        (pathA, pathB) = runJoin $ path <$> blobs
        (oidA, oidB) = runJoin $ oid <$> blobs
        (modeA, modeB) = runJoin $ blobKind <$> blobs

-- | A hunk representing no changes.
emptyHunk :: Hunk (SplitDiff a annotation)
emptyHunk = Hunk { offset = mempty, changes = [], trailingContext = [] }

-- | Render a diff as a series of hunks.
hunks :: HasField fields Range => Diff a (Record fields) -> Both SourceBlob -> [Hunk (SplitDiff a (Record fields))]
hunks _ blobs | sources <- source <$> blobs
              , sourcesEqual <- runBothWith (==) sources
              , sourcesNull <- runBothWith (&&) (null <$> sources)
              , sourcesEqual || sourcesNull
  = [emptyHunk]
hunks diff blobs = hunksInRows (pure 1) $ alignDiff (source <$> blobs) diff

-- | Given beginning line numbers, turn rows in a split diff into hunks in a
-- | patch.
hunksInRows :: Both (Sum Int) -> [Join These (SplitDiff a annotation)] -> [Hunk (SplitDiff a annotation)]
hunksInRows start rows = case nextHunk start rows of
  Nothing -> []
  Just (hunk, rest) -> hunk : hunksInRows (offset hunk <> hunkLength hunk) rest

-- | Given beginning line numbers, return the next hunk and the remaining rows
-- | of the split diff.
nextHunk :: Both (Sum Int) -> [Join These (SplitDiff a annotation)] -> Maybe (Hunk (SplitDiff a annotation), [Join These (SplitDiff a annotation)])
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
nextChange :: Both (Sum Int) -> [Join These (SplitDiff a annotation)] -> Maybe (Both (Sum Int), Change (SplitDiff a annotation), [Join These (SplitDiff a annotation)])
nextChange start rows = case changeIncludingContext leadingContext afterLeadingContext of
  Nothing -> Nothing
  Just (change, afterChanges) -> Just (start <> mconcat (rowIncrement <$> skippedContext), change, afterChanges)
  where (leadingRows, afterLeadingContext) = break rowHasChanges rows
        (skippedContext, leadingContext) = splitAt (max (length leadingRows - 3) 0) leadingRows

-- | Return a Change with the given context and the rows from the begginning of
-- | the given rows that have changes, or Nothing if the first row has no
-- | changes.
changeIncludingContext :: [Join These (SplitDiff a annotation)] -> [Join These (SplitDiff a annotation)] -> Maybe (Change (SplitDiff a annotation), [Join These (SplitDiff a annotation)])
changeIncludingContext leadingContext rows = case changes of
  [] -> Nothing
  _ -> Just (Change leadingContext changes, afterChanges)
  where (changes, afterChanges) = span rowHasChanges rows

-- | Whether a row has changes on either side.
rowHasChanges :: Join These (SplitDiff a annotation) -> Bool
rowHasChanges row = or (hasChanges <$> row)
