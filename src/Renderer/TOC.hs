{-# LANGUAGE ScopedTypeVariables #-}
module Renderer.TOC where

import Category as C
import Data.Aeson
import Data.Functor.Both hiding (fst, snd)
import qualified Data.Functor.Both as Both
import Data.Record
import Diff
import Info
import Prologue
import qualified Data.List as List
import qualified Data.Map as Map hiding (null)
import Renderer
import Source
import Syntax as S
import Term
import Patch
import Unsafe (unsafeHead)

data JSONSummary = JSONSummary { info :: Summarizable }
                 | ErrorSummary { error :: Text, errorSpan :: SourceSpan }
                 deriving (Generic, Eq, Show)

instance ToJSON JSONSummary where
  toJSON JSONSummary{..} = object $ case info of
    InSummarizable{..} -> [ "changeType" .= ("modified" :: Text), "category" .= (show parentCategory :: Text), "term" .= parentTermName, "span" .= parentSourceSpan ]
    Summarizable{..} -> [ "changeType" .= summarizableChangeType, "category" .= (show summarizableCategory :: Text), "term" .= summarizableTermName, "span" .= summarizableSourceSpan ]
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
diffTOC blobs diff = toJSONSummaries =<< removeDupes (diffToTOCSummaries (source <$> blobs) diff)
  where
    removeDupes :: [TOCSummary DiffInfo] -> [TOCSummary DiffInfo]
    removeDupes [] = []
    removeDupes xs = (fmap unsafeHead . List.groupBy (\a b -> parentInfo a == parentInfo b)) xs

    diffToTOCSummaries :: (StringConv leaf Text, DefaultFields fields) => Both (Source Char) -> SyntaxDiff leaf fields -> [TOCSummary DiffInfo]
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
  Just diffInfo -> toTOCSummaries' diffInfo
  Nothing -> panic "No diff"
  where
    toTOCSummaries' = \case
      ErrorInfo{..} -> pure $ TOCSummary patch NotSummarizable
      BranchInfo{..} -> branches >>= toTOCSummaries'
      LeafInfo{..} -> pure . TOCSummary patch $ case leafCategory of
        C.Function -> Summarizable leafCategory termName leafSourceSpan (patchType patch)
        C.Method -> Summarizable leafCategory termName leafSourceSpan (patchType patch)
        _ -> NotSummarizable

mapToInSummarizable :: DefaultFields fields => Both (Source Char) -> SyntaxDiff leaf fields -> [TOCSummary DiffInfo] -> [TOCSummary DiffInfo]
mapToInSummarizable sources diff children = case (beforeTerm diff, afterTerm diff) of
  (_, Just diff') -> mapToInSummarizable' (Both.snd sources) diff' <$> children
  (Just diff', _) -> mapToInSummarizable' (Both.fst sources) diff' <$> children
  (Nothing, Nothing) -> []
  where
    mapToInSummarizable' :: DefaultFields fields => Source Char -> SyntaxTerm leaf fields -> TOCSummary DiffInfo -> TOCSummary DiffInfo
    mapToInSummarizable' source term summary =
      case (parentInfo summary, summarizable term) of
        (NotSummarizable, SummarizableTerm _) ->
          summary { parentInfo = InSummarizable (category (extract term)) (toTermName source term) (Info.sourceSpan (extract term)) }
        (_, _) -> summary

summarizable :: ComonadCofree (Syntax t) w => w a -> SummarizableTerm (w a)
summarizable term = go (unwrap term) term
  where go = \case
          S.Method{} -> SummarizableTerm
          S.Function{} -> SummarizableTerm
          _ -> NotSummarizableTerm

toJSONSummaries :: TOCSummary DiffInfo -> [JSONSummary]
toJSONSummaries TOCSummary{..} = case afterOrBefore summaryPatch of
  Just diffInfo -> toJSONSummaries' diffInfo
  Nothing -> panic "No diff"
  where
    toJSONSummaries' = \case
      ErrorInfo{..} -> pure $ ErrorSummary termName infoSpan
      BranchInfo{..} -> branches >>= toJSONSummaries'
      LeafInfo{..} -> case parentInfo of
        NotSummarizable -> []
        _ -> pure $ JSONSummary parentInfo

termToDiffInfo :: (StringConv leaf Text, DefaultFields fields) => Source Char -> SyntaxTerm leaf fields -> DiffInfo
termToDiffInfo blob term = case unwrap term of
  S.Indexed children -> BranchInfo (termToDiffInfo' <$> children) (category $ extract term)
  S.Fixed children -> BranchInfo (termToDiffInfo' <$> children) (category $ extract term)
  S.AnonymousFunction _ _ -> LeafInfo C.AnonymousFunction (toTermName' term) (getField $ extract term)
  S.Commented cs leaf -> BranchInfo (termToDiffInfo' <$> cs <> maybeToList leaf) (category $ extract term)
  S.Error _ -> ErrorInfo (getField $ extract term) (toTermName' term)
  _ -> toLeafInfo term
  where toTermName' = toTermName blob
        termToDiffInfo' = termToDiffInfo blob
        toLeafInfo term = LeafInfo (category $ extract term) (toTermName' term) (getField $ extract term)

toTermName :: forall leaf fields. DefaultFields fields => Source Char -> SyntaxTerm leaf fields -> Text
toTermName source term = case unwrap term of
  S.Function identifier _ _ -> toTermName' identifier
  S.Method identifier _ _ _ -> toTermName' identifier
  _ -> termNameFromSource term
  where
    toTermName' = toTermName source
    termNameFromSource term = termNameFromRange (range term)
    termNameFromRange range = toText $ Source.slice range source
    range = characterRange . extract
