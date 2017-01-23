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

data JSONSummary = JSONSummary { info :: ParentInfo, changeType :: Text }
                 | ErrorSummary { error :: Text, errorSpan :: SourceSpan }
                 deriving (Generic, Eq, Show)

instance ToJSON JSONSummary where
  -- TODO fix JSON instance
  toJSON JSONSummary{..} = object [ "changeType" .= changeType ]
  toJSON ErrorSummary{..} = object [ "error" .= error, "span" .= errorSpan ]

isErrorSummary :: JSONSummary -> Bool
isErrorSummary ErrorSummary{} = True
isErrorSummary _ = False

data DiffInfo = LeafInfo { leafCategory :: Category, termName :: Text, sourceSpan :: SourceSpan }
 | BranchInfo { branches :: [ DiffInfo ], branchCategory :: Category }
 | ErrorInfo { infoSpan :: SourceSpan, termName :: Text }
 | HideInfo -- Hide/Strip from summary output entirely.
 deriving (Eq, Show)

data TOCSummary a = TOCSummary {
  patch :: Patch a,
  parentAnnotation :: ParentInfo
} deriving (Eq, Functor, Show, Generic)

data ParentInfo = ParentInfo { parentCategory :: Category, parentTermName :: Text, parentSourceSpan :: SourceSpan }
  | ExpressionInfo { exprCategory :: Category, exprTermName :: Text, exprSourceSpan :: SourceSpan }
  | None
  deriving (Eq, Show)

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

diffTOC :: (StringConv leaf Text, DefaultFields fields) => Both SourceBlob -> SyntaxDiff leaf fields -> [JSONSummary]
diffTOC blobs diff = tocToJSONSummaries =<< diffToTOCSummaries (source <$> blobs) diff
  where
    tocToJSONSummaries :: TOCSummary DiffInfo -> [JSONSummary]
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
summaries :: ParentInfo -> Patch DiffInfo -> [JSONSummary]
summaries parentInfo = \case
  (Replace i1 i2) -> zipWith (\a b ->
    let jsonSummary = a
        parentInfo = info a in
      case parentInfo of
        ParentInfo{} -> jsonSummary { info = parentInfo { parentTermName = (parentTermName $ info a) <> " -> " <> (parentTermName $ info b) } }
        ExpressionInfo{} -> jsonSummary { info = parentInfo { exprTermName = (exprTermName $ info a) <> " -> " <> (exprTermName $ info b) } }
        _ -> panic "None ParentInfos should be filtered out"
    ) (toLeafInfos parentInfo "replace" i1) (toLeafInfos parentInfo "replace" i2)
  (Insert info) -> toLeafInfos parentInfo "insert" info
  (Delete info) -> toLeafInfos parentInfo "delete" info

toLeafInfos :: ParentInfo -> Text -> DiffInfo -> [JSONSummary]
toLeafInfos _ _ ErrorInfo{..} = pure $ ErrorSummary termName infoSpan
toLeafInfos parentInfo patchType BranchInfo{..} = branches >>= toLeafInfos parentInfo patchType
toLeafInfos _ _ HideInfo = []
toLeafInfos parentInfo patchType LeafInfo{..} = case leafCategory of
  C.Function -> pure $ JSONSummary (ParentInfo leafCategory termName sourceSpan) patchType
  C.Method -> pure $ JSONSummary (ParentInfo leafCategory termName sourceSpan) patchType
  _ -> case parentInfo of
    None -> []
    info -> pure $ JSONSummary info patchType


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
