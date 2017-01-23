{-# LANGUAGE ScopedTypeVariables #-}
module Renderer.TOC where

import Prologue
import Patch
import Renderer
import Data.Record
import Info
import Data.Functor.Both hiding (fst, snd)
import Source
import Diff


{-

TOCSummary (Patch DiffInfo) [Identifiable (Category, Text), Annotatable]

{
  "changes": [
    { FilePath: [ {SourceSpan, TermContext, TermType, ChangeType} ] }
  ],
  "errors": [
    { FilePath: [ {SourceSpan, ErrorText } ] }
  ]
}
-}

data JSONSummary = JSONSummary { span :: SourceSpan, termContext :: Text, contextType :: Text, changeType :: Text }

data DiffInfo = LeafInfo { leafCategory :: Category, termName :: Text, sourceSpan :: SourceSpan }
 | BranchInfo { branches :: [ DiffInfo ], branchCategory :: Category }
 | ErrorInfo { errorSpan :: SourceSpan, termName :: Text }
 | HideInfo -- Hide/Strip from summary output entirely.
 deriving (Eq, Show)

data TOCSummary a = TOCSummary {
  patch :: Patch a,
  parentAnnotation :: [Either (Category, Text) (Category, Text)]
} deriving (Eq, Functor, Show, Generic)

toc :: (StringConv leaf Text, DefaultFields fields) => Both SourceBlob -> SyntaxDiff leaf fields -> [TOCSummary DiffInfo]
toc blobs diff = tocToJSONSummaries =<< diffToTOC (source <$> blobs) diff

diffToTOC :: (StringConv leaf Text, DefaultFields fields) => Both (Source Char) -> SyntaxDiff leaf fields -> [TOCSummary DiffInfo]
diffToTOC = _

tocToJSONSummaries :: TOCSummary DiffInfo -> [JSONSummary]
tocToJSONSummaries TOCSummary{..} =

-- If the term is a method/function grab the term type, name, and span here, otherwise grab them up top.
summaries :: Patch DiffInfo -> [TOCSummary (Maybe Text) (Maybe Text) (Maybe SourceSpans)]
summaries = \case
  p@(Replace i1 i2) -> zipWith (\a b ->
    TOCSummary
     {
      changeType = "replace"
      termContext =
    , span = SourceSpans $ These (span a) (span b)
    }) (toLeafInfos i1) (toLeafInfos i2)
  p@(Insert info) -> prefixWithPatch p That <$> toLeafInfos info
  p@(Delete info) -> prefixWithPatch p This <$> toLeafInfos info

