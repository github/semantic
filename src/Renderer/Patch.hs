{-# LANGUAGE MultiParamTypeClasses #-}
module Renderer.Patch
( renderPatch
, File(..)
, hunks
, Hunk(..)
, truncatePatch
) where

import Alignment
import Data.Bifunctor.Join
import Data.Blob
import qualified Data.ByteString.Char8 as ByteString
import Data.Functor.Both as Both
import Data.List (span, unzip)
import Data.Range
import Data.Record
import Data.Source as Source hiding (break, drop, length, take)
import Data.These
import Diff
import Patch
import Prologue hiding (fst, snd)
import SplitDiff

-- | Render a timed out file as a truncated diff.
truncatePatch :: Both Blob -> ByteString
truncatePatch blobs = header blobs <> "#timed_out\nTruncating diff: timeout reached.\n"

-- | Render a diff in the traditional patch format.
renderPatch :: (HasField fields Range, Traversable f) => Both Blob -> Diff f (Record fields) -> File
renderPatch blobs diff = File $ if not (ByteString.null text) && ByteString.last text /= '\n'
  then text <> "\n\\ No newline at end of file\n"
  else text
  where text = header blobs <> mconcat (showHunk blobs <$> hunks diff blobs)

newtype File = File { unFile :: ByteString }
  deriving Show

instance Monoid File where
  mempty = File mempty
  mappend (File a) (File b) = File (a <> "\n" <> b)

instance StringConv File ByteString where
  strConv _ = unFile


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
showHunk :: Functor f => HasField fields Range => Both Blob -> Hunk (SplitDiff f (Record fields)) -> ByteString
showHunk blobs hunk = maybeOffsetHeader <>
  mconcat (showChange sources <$> changes hunk) <>
  showLines (snd sources) ' ' (maybeSnd . runJoin <$> trailingContext hunk)
  where sources = blobSource <$> blobs
        maybeOffsetHeader = if lengthA > 0 && lengthB > 0
                            then offsetHeader
                            else mempty
        offsetHeader = "@@ -" <> offsetA <> "," <> show lengthA <> " +" <> offsetB <> "," <> show lengthB <> " @@" <> "\n"
        (lengthA, lengthB) = runJoin . fmap getSum $ hunkLength hunk
        (offsetA, offsetB) = runJoin . fmap (show . getSum) $ offset hunk

-- | Given the before and after sources, render a change to a string.
showChange :: Functor f => HasField fields Range => Both Source -> Change (SplitDiff f (Record fields)) -> ByteString
showChange sources change = showLines (snd sources) ' ' (maybeSnd . runJoin <$> context change) <> deleted <> inserted
  where (deleted, inserted) = runJoin $ pure showLines <*> sources <*> both '-' '+' <*> Join (unzip (fromThese Nothing Nothing . runJoin . fmap Just <$> contents change))

-- | Given a source, render a set of lines to a string with a prefix.
showLines :: Functor f => HasField fields Range => Source -> Char -> [Maybe (SplitDiff f (Record fields))] -> ByteString
showLines source prefix lines = fromMaybe "" . mconcat $ fmap prepend . showLine source <$> lines
  where prepend "" = ""
        prepend source = ByteString.singleton prefix <> source

-- | Given a source, render a line to a string.
showLine :: Functor f => HasField fields Range => Source -> Maybe (SplitDiff f (Record fields)) -> Maybe ByteString
showLine source line | Just line <- line = Just . sourceText . (`slice` source) $ getRange line
                     | otherwise = Nothing

-- | Returns the header given two source blobs and a hunk.
header :: Both Blob -> ByteString
header blobs = ByteString.intercalate "\n" ([filepathHeader, fileModeHeader] <> maybeFilepaths) <> "\n"
  where filepathHeader = "diff --git a/" <> pathA <> " b/" <> pathB
        fileModeHeader = case (modeA, modeB) of
          (Nothing, Just mode) -> ByteString.intercalate "\n" [ "new file mode " <> modeToDigits mode, blobOidHeader ]
          (Just mode, Nothing) -> ByteString.intercalate "\n" [ "deleted file mode " <> modeToDigits mode, blobOidHeader ]
          (Just mode, Just other) | mode == other -> "index " <> oidA <> ".." <> oidB <> " " <> modeToDigits mode
          (Just mode1, Just mode2) -> ByteString.intercalate "\n" [
            "old mode " <> modeToDigits mode1,
            "new mode " <> modeToDigits mode2,
            blobOidHeader
            ]
          (Nothing, Nothing) -> ""
        blobOidHeader = "index " <> oidA <> ".." <> oidB
        modeHeader :: ByteString -> Maybe BlobKind -> ByteString -> ByteString
        modeHeader ty maybeMode path = case maybeMode of
           Just _ -> ty <> "/" <> path
           Nothing -> "/dev/null"
        maybeFilepaths = if (nullOid == oidA && nullSource (snd sources)) || (nullOid == oidB && nullSource (fst sources)) then [] else [ beforeFilepath, afterFilepath ]
        beforeFilepath = "--- " <> modeHeader "a" modeA pathA
        afterFilepath = "+++ " <> modeHeader "b" modeB pathB
        sources = blobSource <$> blobs
        (pathA, pathB) = case runJoin $ toS . blobPath <$> blobs of
          ("", path) -> (path, path)
          (path, "") -> (path, path)
          paths -> paths
        (oidA, oidB) = runJoin $ blobOid <$> blobs
        (modeA, modeB) = runJoin $ blobKind <$> blobs

-- | A hunk representing no changes.
emptyHunk :: Hunk (SplitDiff a annotation)
emptyHunk = Hunk { offset = mempty, changes = [], trailingContext = [] }

-- | Render a diff as a series of hunks.
hunks :: (Traversable f, HasField fields Range) => Diff f (Record fields) -> Both Blob -> [Hunk (SplitDiff [] (Record fields))]
hunks _ blobs | sources <- blobSource <$> blobs
              , sourcesEqual <- runBothWith (==) sources
              , sourcesNull <- runBothWith (&&) (nullSource <$> sources)
              , sourcesEqual || sourcesNull
  = [emptyHunk]
hunks diff blobs = hunksInRows (pure 1) $ alignDiff (blobSource <$> blobs) diff

-- | Given beginning line numbers, turn rows in a split diff into hunks in a
-- | patch.
hunksInRows :: (Foldable f, Functor f) => Both (Sum Int) -> [Join These (SplitDiff f annotation)] -> [Hunk (SplitDiff f annotation)]
hunksInRows start rows = case nextHunk start rows of
  Nothing -> []
  Just (hunk, rest) -> hunk : hunksInRows (offset hunk <> hunkLength hunk) rest

-- | Given beginning line numbers, return the next hunk and the remaining rows
-- | of the split diff.
nextHunk :: (Foldable f, Functor f) => Both (Sum Int) -> [Join These (SplitDiff f annotation)] -> Maybe (Hunk (SplitDiff f annotation), [Join These (SplitDiff f annotation)])
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
nextChange :: (Foldable f, Functor f) => Both (Sum Int) -> [Join These (SplitDiff f annotation)] -> Maybe (Both (Sum Int), Change (SplitDiff f annotation), [Join These (SplitDiff f annotation)])
nextChange start rows = case changeIncludingContext leadingContext afterLeadingContext of
  Nothing -> Nothing
  Just (change, afterChanges) -> Just (start <> mconcat (rowIncrement <$> skippedContext), change, afterChanges)
  where (leadingRows, afterLeadingContext) = break rowHasChanges rows
        (skippedContext, leadingContext) = splitAt (max (length leadingRows - 3) 0) leadingRows

-- | Return a Change with the given context and the rows from the begginning of
-- | the given rows that have changes, or Nothing if the first row has no
-- | changes.
changeIncludingContext :: (Foldable f, Functor f) => [Join These (SplitDiff f annotation)] -> [Join These (SplitDiff f annotation)] -> Maybe (Change (SplitDiff f annotation), [Join These (SplitDiff f annotation)])
changeIncludingContext leadingContext rows = case changes of
  [] -> Nothing
  _ -> Just (Change leadingContext changes, afterChanges)
  where (changes, afterChanges) = span rowHasChanges rows

-- | Whether a row has changes on either side.
rowHasChanges :: (Foldable f, Functor f) => Join These (SplitDiff f annotation) -> Bool
rowHasChanges row = or (hasChanges <$> row)
