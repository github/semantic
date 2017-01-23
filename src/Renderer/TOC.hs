{-# LANGUAGE ScopedTypeVariables #-}
module Renderer.TOC where

import Category as C
import Data.Aeson
import Data.Functor.Both hiding (fst, snd)
import Data.Record
import Diff
import Info
import Patch
import Prologue
import qualified Data.List as List
import qualified Data.Map as Map hiding (null)
import Renderer
import Source
import Syntax as S
import Term


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

Example: https://github.com/github/github/pull/50259/files

* app/controllers/application_controller/feature_flags_dependency.rb
    * enable_feature_flags (method)

* app/models/linked_account_collection.rb
    * remove (method)

* app/models/repository.rb
    * filtered_by (method)

* app/models/tenant/mismatch_check.rb
    + report_tenant_mismatch? (method)
    + report_tenant_mismatch (method)

* app/models/user.rb
    * recently_updated_member_repos (method)

* lib/github/config.rb
    + report_tenant_mismatch_enabled? (method)

-}

data JSONSummary span termContext contextType = JSONSummary { span :: span, termContext :: termContext, contextType :: contextType, changeType :: Text }
                 | ErrorSummary { error :: Text, errorSpan :: SourceSpan }
                 deriving (Generic, Eq, Show)

instance (ToJSON span, ToJSON termContext, ToJSON contextType) => ToJSON (JSONSummary span termContext contextType) where
  toJSON JSONSummary{..} = object [ "span" .= span, "termContext" .= termContext, "contextType" .= contextType, "changeType" .= changeType ]
  toJSON ErrorSummary{..} = object [ "error" .= error, "span" .= errorSpan ]

isErrorSummary :: JSONSummary termContext contextType changeType -> Bool
isErrorSummary ErrorSummary{} = True
isErrorSummary _ = False

data DiffInfo = LeafInfo { leafCategory :: Category, termName :: Text, sourceSpan :: SourceSpan }
 | BranchInfo { branches :: [ DiffInfo ], branchCategory :: Category }
 | ErrorInfo { infoSpan :: SourceSpan, termName :: Text }
 | HideInfo -- Hide/Strip from summary output entirely.
 deriving (Eq, Show)

data TOCSummary a = TOCSummary {
  patch :: Patch a,
  parentAnnotation :: [Either (Category, Text) (Category, Text)]
} deriving (Eq, Functor, Show, Generic)

toc :: (DefaultFields fields) => Renderer (Record fields)
toc blobs diff = TOCOutput $ Map.fromList [
    ("changes", changes),
    ("errors", errors)
  ]
  where
    changes = if null changes' then mempty else Map.singleton summaryKey (toJSON <$> changes')
    errors = if null errors' then mempty else Map.singleton summaryKey (toJSON <$> errors')
    (errors', changes') = List.partition isErrorSummary summaries
    summaryKey = toSummaryKey (path <$> blobs)
    summaries = diffTOC blobs diff

diffTOC :: (StringConv leaf Text, DefaultFields fields) => Both SourceBlob -> SyntaxDiff leaf fields -> [JSONSummary SourceSpan Text Text]
diffTOC blobs diff = tocToJSONSummaries =<< diffToTOCSummaries (source <$> blobs) diff
  where
    tocToJSONSummaries :: TOCSummary DiffInfo -> [JSONSummary SourceSpan Text Text]
    tocToJSONSummaries TOCSummary{..} = summaries parentAnnotation patch

    diffToTOCSummaries :: (StringConv leaf Text, DefaultFields fields) => Both (Source Char) -> SyntaxDiff leaf fields -> [TOCSummary DiffInfo]
    diffToTOCSummaries sources = para $ \diff ->
      let
        -- TODO Turn this on to add annotateWithCategory back
        -- diff' = free (Prologue.fst <$> diff)
        -- annotateWithCategory :: [TOCSummary DiffInfo] -> [TOCSummary DiffInfo]
        -- annotateWithCategory children = case (beforeTerm diff', afterTerm diff') of
        --   (_, Just diff'') -> appendSummary (Both.snd sources) diff'' <$> children
        --   (Just diff'', _) -> appendSummary (Both.fst sources) diff'' <$> children
        --   (Nothing, Nothing) -> []
      in case diff of
        -- Skip comments and leaves since they don't have any changes
        -- (Free (_ :< syntax)) -> annotateWithCategory (toList syntax >>= snd)
        (Free (_ :< syntax)) -> toList syntax >>= snd
        (Pure patch) -> [ TOCSummary (mapPatch (termToDiffInfo beforeSource) (termToDiffInfo afterSource) patch) [] ]
      where
        (beforeSource, afterSource) = runJoin sources



-- If the term is a method/function grab the term type, name, and span here, otherwise grab them up top.
summaries :: [Either (Category, Text) (Category, Text)] -> Patch DiffInfo -> [JSONSummary (Maybe SourceSpan) (Maybe Text) (Maybe Text)]
summaries parentAnnotations = \case
  (Replace i1 i2) -> zipWith (\a b ->
    case (contextType a, termContext a, termContext b, contextType b) of
      (Just C.Function, Just contextA, Just contextB, Just context) ->
        JSONSummary (span b) (contextA <> " -> " <> contextB) context (changeType b)
      (Just C.Method, Just contextA, Just contextB, Just context) ->
        JSONSummary (span b) (contextA <> " -> " <> contextB) context (changeType b)
      _ -> JSONSummary Nothing Nothing Nothing (changeType b)
    ) (toLeafInfos parentAnnotations "replace" i1) (toLeafInfos parentAnnotations "replace" i2)
  (Insert info) -> toLeafInfos parentAnnotations "insert" info
  (Delete info) -> toLeafInfos parentAnnotations "delete" info

toLeafInfos :: [Either (Category, Text) (Category, Text)] -> Text -> DiffInfo -> [JSONSummary (Maybe SourceSpan) (Maybe Text) (Maybe Category)]
toLeafInfos _ _ ErrorInfo{..} = pure $ ErrorSummary termName infoSpan
toLeafInfos parentAnnotations patchType BranchInfo{..} = branches >>= toLeafInfos parentAnnotations patchType
toLeafInfos _ _ HideInfo = []
toLeafInfos parentAnnotations patchType LeafInfo{..} = pure $ case leafCategory of
  C.Function -> JSONSummary (Just sourceSpan) (Just termName) (Just leafCategory) patchType
  C.Method -> JSONSummary (Just sourceSpan) (Just termName) (Just leafCategory) patchType
  _ -> JSONSummary Nothing Nothing Nothing patchType

termToDiffInfo :: (StringConv leaf Text, DefaultFields fields) => Source Char -> SyntaxTerm leaf fields -> DiffInfo
termToDiffInfo blob term = case unwrap term of
  S.Indexed children -> BranchInfo (termToDiffInfo' <$> children) (category $ extract term)
  S.Fixed children -> BranchInfo (termToDiffInfo' <$> children) (category $ extract term)
  S.AnonymousFunction _ _ -> LeafInfo C.AnonymousFunction (toTermName' term) (getField $ extract term)
  S.Comment _ -> HideInfo
  S.Commented cs leaf -> BranchInfo (termToDiffInfo' <$> cs <> maybeToList leaf) (category $ extract term)
  S.Error _ -> ErrorInfo (getField $ extract term) (toTermName' term)
  _ -> toLeafInfo term
  where toTermName' = toTermName blob
        termToDiffInfo' = termToDiffInfo blob
        toLeafInfo term = LeafInfo (category $ extract term) (toTermName' term) (getField $ extract term)

toTermName :: forall leaf fields. (StringConv leaf Text, DefaultFields fields) => Source Char -> SyntaxTerm leaf fields -> Text
toTermName source = termNameFromSource
  where
    termNameFromSource term = termNameFromRange (range term)
    termNameFromRange range = toText $ Source.slice range source
    range = characterRange . extract
