{-# LANGUAGE ScopedTypeVariables #-}
module Renderer.TOC (toc, diffTOC, JSONSummary(..), Summarizable(..), isErrorSummary) where

import Category as C
import Data.Aeson
import Data.Functor.Both hiding (fst, snd)
import qualified Data.Functor.Both as Both
import Data.Text (toLower)
import Data.Record
import Diff
import Info
import Prologue
import Range
import qualified Data.List as List
import qualified Data.Map as Map hiding (null)
import Renderer
import Source hiding (null)
import Syntax as S
import Term
import Patch

data JSONSummary = JSONSummary { info :: Summarizable }
                 | ErrorSummary { error :: Text, errorSpan :: SourceSpan }
                 deriving (Generic, Eq, Show)

instance ToJSON JSONSummary where
  toJSON JSONSummary{..} = object $ case info of
    InSummarizable{..} -> [ "changeType" .= ("modified" :: Text), "category" .= toCategoryName parentCategory, "term" .= parentTermName, "span" .= parentSourceSpan ]
    Summarizable{..} -> [ "changeType" .= summarizableChangeType, "category" .= toCategoryName summarizableCategory, "term" .= summarizableTermName, "span" .= summarizableSourceSpan ]
    NotSummarizable -> panic "NotSummarizable should have been pruned"
  toJSON ErrorSummary{..} = object [ "error" .= error, "span" .= errorSpan ]

isErrorSummary :: JSONSummary -> Bool
isErrorSummary ErrorSummary{} = True
isErrorSummary _ = False

data DiffInfo = LeafInfo { leafCategory :: Category, termName :: Text, leafSourceSpan :: SourceSpan }
 | BranchInfo { branches :: [ DiffInfo ], branchCategory :: Category }
 | ErrorInfo { infoSpan :: SourceSpan, termName :: Text }
 deriving (Eq, Show)

data TOCSummary a = TOCSummary {
                      summaryPatch :: Patch a,
                      parentInfo :: Summarizable
                    } deriving (Eq, Functor, Show, Generic)

data Summarizable = Summarizable { summarizableCategory :: Category, summarizableTermName :: Text, summarizableSourceSpan :: SourceSpan, summarizableChangeType :: Text }
  | InSummarizable { parentCategory :: Category, parentTermName :: Text, parentSourceSpan :: SourceSpan }
  | NotSummarizable
  deriving (Eq, Show)

data SummarizableTerm a = SummarizableTerm a | NotSummarizableTerm a

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
diffTOC blobs diff = removeDupes (diffToTOCSummaries (source <$> blobs) diff) >>= toJSONSummaries
  where
    removeDupes :: [TOCSummary DiffInfo] -> [TOCSummary DiffInfo]
    removeDupes = foldl' go []
      where
        go xs x | (_, _ : _) <- find exactMatch x xs = xs
                | (front, existingItem : back) <- find similarMatch x xs =
                   let
                     (Summarizable category name sourceSpan _) = parentInfo existingItem
                     replacement = x { parentInfo = Summarizable category name sourceSpan "modified" }
                   in
                     front <> (replacement : back)
                | otherwise = xs <> [x]
        find p x = List.break (p x)
        exactMatch a b = parentInfo a == parentInfo b
        similarMatch a b = case (parentInfo a, parentInfo b) of
          (Summarizable catA nameA _ _, Summarizable catB nameB _ _) -> catA == catB && toLower nameA == toLower nameB
          (_, _) -> False

    diffToTOCSummaries :: (StringConv leaf Text, DefaultFields fields) => Both Source -> SyntaxDiff leaf fields -> [TOCSummary DiffInfo]
    diffToTOCSummaries sources = para $ \diff ->
      let
        diff' = free (Prologue.fst <$> diff)
        patch' = mapPatch (termToDiffInfo beforeSource) (termToDiffInfo afterSource)
        (beforeSource, afterSource) = runJoin sources
      in case diff of
        (Free (_ :< syntax)) -> mapToInSummarizable sources diff' (toList syntax >>= snd)
        (Pure patch) -> toTOCSummaries (patch' patch)

-- Mark which leaves are summarizable.
toTOCSummaries :: Patch DiffInfo -> [TOCSummary DiffInfo]
toTOCSummaries patch = case afterOrBefore patch of
  Just diffInfo -> toTOCSummaries' patch diffInfo
  Nothing -> panic "No diff"
  where
    toTOCSummaries' patch' diffInfo = case diffInfo of
      ErrorInfo{..} -> pure $ TOCSummary patch' NotSummarizable
      BranchInfo{..} -> join $ zipWith toTOCSummaries' (flattenPatch patch') branches
      LeafInfo{..} -> pure . TOCSummary patch' $ case leafCategory of
        C.Function -> Summarizable leafCategory termName leafSourceSpan (patchType patch')
        C.Method -> Summarizable leafCategory termName leafSourceSpan (patchType patch')
        C.SingletonMethod -> Summarizable leafCategory termName leafSourceSpan (patchType patch')
        _ -> NotSummarizable

flattenPatch :: Patch DiffInfo -> [Patch DiffInfo]
flattenPatch patch = case patch of
  Replace i1 i2 -> zipWith Replace (toLeafInfos' i1) (toLeafInfos' i2)
  Insert info -> Insert <$> toLeafInfos' info
  Delete info -> Delete <$> toLeafInfos' info

toLeafInfos' :: DiffInfo -> [DiffInfo]
toLeafInfos' BranchInfo{..} = branches >>= toLeafInfos'
toLeafInfos' leaf = [leaf]

mapToInSummarizable :: forall leaf fields. DefaultFields fields => Both Source -> SyntaxDiff leaf fields -> [TOCSummary DiffInfo] -> [TOCSummary DiffInfo]
mapToInSummarizable sources diff children = case (beforeTerm diff, afterTerm diff) of
  (_, Just diff') -> mapToInSummarizable' (Both.snd sources) diff' <$> children
  (Just diff', _) -> mapToInSummarizable' (Both.fst sources) diff' <$> children
  (Nothing, Nothing) -> []
  where
    mapToInSummarizable' :: Source -> SyntaxTerm leaf fields -> TOCSummary DiffInfo -> TOCSummary DiffInfo
    mapToInSummarizable' source term summary =
      case (parentInfo summary, summarizable term) of
        (NotSummarizable, SummarizableTerm _) ->
          summary { parentInfo = InSummarizable (category (extract term)) (toTermName 0 source term) (Info.sourceSpan (extract term)) }
        (_, _) -> summary

summarizable :: ComonadCofree (Syntax t) w => w a -> SummarizableTerm (w a)
summarizable term = go (unwrap term) term
  where go syntax = case syntax of
          S.Method{} -> SummarizableTerm
          S.Function{} -> SummarizableTerm
          _ -> NotSummarizableTerm

toJSONSummaries :: TOCSummary DiffInfo -> [JSONSummary]
toJSONSummaries TOCSummary{..} = case afterOrBefore summaryPatch of
  Just diffInfo -> toJSONSummaries' diffInfo
  Nothing -> panic "No diff"
  where
    toJSONSummaries' diffInfo = case diffInfo of
      ErrorInfo{..} -> pure $ ErrorSummary termName infoSpan
      BranchInfo{..} -> branches >>= toJSONSummaries'
      LeafInfo{..} -> case parentInfo of
        NotSummarizable -> []
        _ -> pure $ JSONSummary parentInfo

termToDiffInfo :: forall leaf fields. (StringConv leaf Text, DefaultFields fields) => Source -> SyntaxTerm leaf fields -> DiffInfo
termToDiffInfo source term = case unwrap term of
  S.Indexed children -> BranchInfo (termToDiffInfo' <$> children) (category $ extract term)
  S.Fixed children -> BranchInfo (termToDiffInfo' <$> children) (category $ extract term)
  S.AnonymousFunction _ _ -> LeafInfo C.AnonymousFunction (toTermName' term) (getField $ extract term)
  S.Commented cs leaf -> BranchInfo (termToDiffInfo' <$> cs <> maybeToList leaf) (category $ extract term)
  S.ParseError _ -> ErrorInfo (getField $ extract term) (toTermName' term)
  _ -> toLeafInfo term
  where
    toTermName' = toTermName 0 source
    termToDiffInfo' = termToDiffInfo source
    toLeafInfo term = LeafInfo (category $ extract term) (toTermName' term) (getField $ extract term)

toTermName :: forall leaf fields. DefaultFields fields => Int -> Source -> SyntaxTerm leaf fields -> Text
toTermName parentOffset parentSource term = case unwrap term of
  S.Function identifier _ _ -> toTermName' identifier
  S.Method identifier Nothing _ _ -> toTermName' identifier
  S.Method identifier (Just receiver) _ _ -> case unwrap receiver of
    S.Indexed [receiverParams] -> case unwrap receiverParams of
      S.ParameterDecl (Just ty) _ -> "(" <> toTermName' ty <> ") " <> toTermName' identifier
      _ -> toMethodNameWithReceiver receiver identifier
    _ -> toMethodNameWithReceiver receiver identifier
  _ -> toText source
  where
    source = Source.slice (offsetRange (range term) (negate parentOffset)) parentSource
    toMethodNameWithReceiver receiver name = toTermName' receiver <> "." <> toTermName' name
    offset = start (range term)
    toTermName' :: SyntaxTerm leaf fields -> Text
    toTermName' = toTermName offset source
    range = byteRange . extract

-- The user-facing category name
toCategoryName :: Category -> Text
toCategoryName category = case category of
  C.SingletonMethod -> "Method"
  c -> show c
