module Renderer (Renderer, DiffArguments(..), Output(..), concatOutputs, toSummaryKey, Format(..)) where

import Prologue
import Data.Functor.Both
import Diff
import Source (SourceBlob)
import Data.Text as T (intercalate)
import Data.Aeson (Object, toEncoding)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.HashMap.Strict as HashMap

-- | A function that will render a diff, given the two source blobs.
type Renderer annotation = Both SourceBlob -> Diff Text annotation -> Output

data DiffArguments = DiffArguments { format :: Format, output :: Maybe FilePath, outputPath :: FilePath }
 deriving (Show)

-- | The available types of diff rendering.
data Format = Split | Patch | JSON | Summary
  deriving (Show)

data Output = SplitOutput Text | PatchOutput Text | JSONOutput Object  | SummaryOutput (HashMap Text [Text])
  deriving (Show)

toSummaryKey :: Both FilePath -> Text
toSummaryKey = runBothWith $ \before after ->
  toS $ if before == after then after else before <> " -> " <> after

concatOutputs :: [Output] -> Text
concatOutputs list | isJSON list = toS . encodingToLazyByteString . toEncoding $ concatJSON list
concatOutputs list | isSummary list = toS . encodingToLazyByteString . toEncoding $ concatSummaries list
concatOutputs list | isText list = T.intercalate "\n" (toText <$> list)
concatOutputs _ = mempty

concatJSON :: [Output] -> Object
concatJSON (JSONOutput hash : rest) = HashMap.union hash (concatJSON rest)
concatJSON _ = mempty

concatSummaries :: [Output] -> HashMap Text [Text]
concatSummaries (SummaryOutput hash : rest) = HashMap.unionWith (<>) hash (concatSummaries rest)
concatSummaries _ = mempty

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
