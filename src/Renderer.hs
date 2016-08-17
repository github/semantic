module Renderer (Renderer, DiffArguments(..), Output(..), concatOutputs, Format(..)) where

import Prologue
import Data.Functor.Both
import Diff
import Source (SourceBlob)
import Data.Aeson.Types (Series, pairs)
import Data.Text as T (intercalate)
import Data.Aeson.Encoding (encodingToLazyByteString)

-- | A function that will render a diff, given the two source blobs.
type Renderer annotation = Both SourceBlob -> Diff Text annotation -> Output

data DiffArguments = DiffArguments { format :: Format, output :: Maybe FilePath, outputPath :: FilePath }
 deriving (Show)

data Output = SplitOutput Text | PatchOutput Text | JSONOutput Series  | SummaryOutput Series

concatOutputs :: [Output] -> Text
concatOutputs l@(JSONOutput _ : _) = toS . encodingToLazyByteString . pairs . mconcat $ toSeries <$> l
concatOutputs l@(SummaryOutput _ : _) = toS . encodingToLazyByteString . pairs . mconcat $  toSeries <$> l
concatOutputs l = T.intercalate "\n" (toText <$> l)

toSeries :: Output -> Series
toSeries (JSONOutput series) = series
toSeries (SummaryOutput series) = series
toSeries _ = mempty

toText :: Output -> Text
toText (SplitOutput text) = text
toText (PatchOutput text) = text
toText _ = mempty


-- | The available types of diff rendering.
data Format = Split | Patch | JSON | Summary
  deriving (Show)
