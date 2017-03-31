{-# LANGUAGE GADTs #-}
module Renderer
( DiffRenderer(..)
, runDiffRenderer
, Renderer
, Output(..)
, concatOutputs
, Format(..)
) where

import Data.Aeson (ToJSON, Value, encode)
import Data.Functor.Both
import Data.Map as Map hiding (null)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import Data.Functor.Listable
import Data.Record
import Info
import Prologue
import Renderer.JSON as R
import Renderer.Patch as R
import Renderer.SExpression as R
import Renderer.Split as R
import Renderer.Summary as R
import Renderer.TOC as R
import Source (SourceBlob)
import Syntax
import Diff

data DiffRenderer fields output where
  SplitRenderer :: (HasField fields Category, HasField fields Range) => DiffRenderer fields Text
  PatchRenderer :: HasField fields Range => DiffRenderer fields Text
  JSONDiffRenderer :: (ToJSON (Record fields), HasField fields Category, HasField fields Range) => DiffRenderer fields (Map Text Value)
  SummaryRenderer :: HasDefaultFields fields => DiffRenderer fields (Map Text (Map Text [Value]))
  SExpressionDiffRenderer :: (HasField fields Category, HasField fields SourceSpan) => SExpressionFormat -> DiffRenderer fields ByteString
  ToCRenderer :: HasDefaultFields fields => DiffRenderer fields (Map Text (Map Text [Value]))

runDiffRenderer :: Both SourceBlob -> Diff (Syntax Text) (Record fields) -> DiffRenderer fields output -> Output
runDiffRenderer sources diff renderer = case renderer of
  SplitRenderer -> SplitOutput (R.split sources diff)
  PatchRenderer -> PatchOutput (R.patch sources diff)
  JSONDiffRenderer -> JSONOutput (R.json sources diff)
  SummaryRenderer -> SummaryOutput (R.summary sources diff)
  SExpressionDiffRenderer format -> SExpressionOutput (R.sExpression format sources diff)
  ToCRenderer -> TOCOutput (R.toc sources diff)

data TermRenderer fields output where
  JSONTermRenderer :: TermRenderer fields (Map Text Value)
  SExpressionTermRenderer :: (HasField fields Category, HasField fields SourceSpan) => SExpressionFormat -> TermRenderer fields ByteString

-- | A function that will render a diff, given the two source blobs.
type Renderer annotation = Both SourceBlob -> Diff (Syntax Text) annotation -> Output

-- | The available types of diff rendering.
data Format = Split | Patch | JSON | Summary | SExpression | TOC | Index | ParseTree
  deriving (Show)

data Output = SplitOutput Text | PatchOutput Text | JSONOutput (Map Text Value) | SummaryOutput (Map Text (Map Text [Value])) | SExpressionOutput ByteString | TOCOutput (Map Text (Map Text [Value]))
  deriving (Show)

-- Concatenates a list of 'Output' depending on the output type.
-- For JSON, each file output is merged since they're uniquely keyed by filename.
-- For Summaries, each file output is merged into one 'Object' consisting of lists of
-- changes and errors.
-- Split and Patch output is appended together with newlines.
concatOutputs :: [Output] -> ByteString
concatOutputs list | isJSON list = toS . encode $ concatJSON list
  where
    concatJSON :: [Output] -> Map Text Value
    concatJSON (JSONOutput hash : rest) = Map.union hash (concatJSON rest)
    concatJSON _ = mempty
concatOutputs list | isSummary list = toS . encode $ concatSummaries list
  where
    concatSummaries :: [Output] -> Map Text (Map Text [Value])
    concatSummaries (SummaryOutput hash : rest) = Map.unionWith (Map.unionWith (<>)) hash (concatSummaries rest)
    concatSummaries (TOCOutput hash : rest) = Map.unionWith (Map.unionWith (<>)) hash (concatSummaries rest)
    concatSummaries _ = mempty
concatOutputs list | isByteString list = B.intercalate "\n" (toByteString <$> list)
concatOutputs list | isText list = B.intercalate "\n" (encodeUtf8 . toText <$> list)
concatOutputs _ = mempty

isJSON :: [Output] -> Bool
isJSON (JSONOutput _ : _) = True
isJSON _ = False

isSummary :: [Output] -> Bool
isSummary (SummaryOutput _ : _) = True
isSummary (TOCOutput _ : _) = True
isSummary _ = False

isText :: [Output] -> Bool
isText (SplitOutput _ : _) = True
isText (PatchOutput _ : _) = True
isText _ = False

toText :: Output -> Text
toText (SplitOutput text) = text
toText (PatchOutput text) = text
toText _ = mempty

isByteString :: [Output] -> Bool
isByteString (SExpressionOutput _ : _) = True
isByteString _ = False

toByteString :: Output -> ByteString
toByteString (SExpressionOutput text) = text
toByteString _ = B.empty


instance Listable Format where
  tiers = cons0 Split
       \/ cons0 Patch
       \/ cons0 JSON
       \/ cons0 Summary
       \/ cons0 SExpression
       \/ cons0 TOC
