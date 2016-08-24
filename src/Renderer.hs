module Renderer (Renderer, DiffArguments(..), Output(..), concatOutputs, toSummaryKey, Format(..)) where

import Prologue
import Data.Functor.Both
import Diff
import Source (SourceBlob)
import Data.Text as T (intercalate)
import Data.Aeson (Value, toEncoding)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Map as Map

-- | A function that will render a diff, given the two source blobs.
type Renderer annotation = Both SourceBlob -> Diff Text annotation -> Output

data DiffArguments = DiffArguments { format :: Format, output :: Maybe FilePath, outputPath :: FilePath }
 deriving (Show)

-- | The available types of diff rendering.
data Format = Split | Patch | JSON | Summary
  deriving (Show)

data Output = SplitOutput Text | PatchOutput Text | JSONOutput (Map Text Value)  | SummaryOutput (Map Text [Text])
  deriving (Show)

toSummaryKey :: Both FilePath -> Text
toSummaryKey = runBothWith $ \before after ->
  toS $ if before == after then after else before <> " -> " <> after

-- Concatenates a list of 'Output' depending on the output type.
-- For JSON, each file output is merged since they're uniquely keyed by filename.
-- For Summaries, each file output is merged into one 'Object' consisting of lists of
-- changes and errors.
-- Split and Patch output is appended together with newlines.
concatOutputs :: [Output] -> Text
concatOutputs list | isJSON list = toS . encodingToLazyByteString . toEncoding $ concatJSON list
  where
    concatJSON :: [Output] -> Map Text Value
    concatJSON (JSONOutput hash : rest) = Map.union hash (concatJSON rest)
    concatJSON _ = mempty
concatOutputs list | isSummary list = toS . encodingToLazyByteString . toEncoding $ concatSummaries list
  where
    concatSummaries :: [Output] -> Map Text [Text]
    concatSummaries (SummaryOutput hash : rest) = Map.unionWith (<>) hash (concatSummaries rest)
    concatSummaries _ = mempty
concatOutputs list | isText list = T.intercalate "\n" (toText <$> list)
concatOutputs _ = mempty

isJSON :: [Output] -> Bool
isJSON (JSONOutput _ : _) = True
isJSON _ = False

isSummary :: [Output] -> Bool
isSummary (SummaryOutput _ : _) = True
isSummary _ = False

isText :: [Output] -> Bool
isText (SplitOutput _ : _) = True
isText (PatchOutput _ : _) = True
isText _ = False

toText :: Output -> Text
toText (SplitOutput text) = text
toText (PatchOutput text) = text
toText _ = mempty
