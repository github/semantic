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
import Renderer.Summary (Summaries(..))
import qualified Data.List as List
import qualified Data.Map as Map hiding (null)
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
  toJSON ErrorSummary{..} = object [ "error" .= error, "span" .= errorSpan ]

isErrorSummary :: JSONSummary -> Bool
isErrorSummary ErrorSummary{} = True
isErrorSummary _ = False

data DiffInfo
  = LeafInfo
    { leafCategory :: Category
    , termName :: Text
    , leafSourceSpan :: SourceSpan
    }
  | ErrorInfo
    { infoSpan :: SourceSpan
    , termName :: Text
    }
  deriving (Eq, Show)

data TOCSummary a = TOCSummary {
                      summaryPatch :: Patch a,
                      parentInfo :: Maybe Summarizable
                    } deriving (Eq, Functor, Show, Generic)

data Summarizable
  = Summarizable
    { summarizableCategory :: Category
    , summarizableTermName :: Text
    , summarizableSourceSpan :: SourceSpan
    , summarizableChangeType :: Text
    }
  | InSummarizable
    { parentCategory :: Category
    , parentTermName :: Text
    , parentSourceSpan :: SourceSpan
    }
  deriving (Eq, Show)

toc :: HasDefaultFields fields => Both SourceBlob -> Diff (Syntax Text) (Record fields) -> Summaries
toc blobs diff = Summaries changes errors
  where
    changes = if null changes' then mempty else Map.singleton summaryKey (toJSON <$> changes')
    errors = if null errors' then mempty else Map.singleton summaryKey (toJSON <$> errors')
    (errors', changes') = List.partition isErrorSummary summaries
    summaries = diffTOC blobs diff

    summaryKey = toS $ case runJoin (path <$> blobs) of
      (before, after) | null before -> after
                      | null after -> before
                      | before == after -> after
                      | otherwise -> before <> " -> " <> after

diffTOC :: HasDefaultFields fields => Both SourceBlob -> Diff (Syntax Text) (Record fields) -> [JSONSummary]
diffTOC blobs diff = removeDupes (diffToTOCSummaries (source <$> blobs) diff) >>= toJSONSummaries
  where
    removeDupes :: [TOCSummary DiffInfo] -> [TOCSummary DiffInfo]
    removeDupes = foldl' go []
      where
        go xs x | (_, _ : _) <- find exactMatch x xs = xs
                | (front, existingItem : back) <- find similarMatch x xs =
                   let
                     Just (Summarizable category name sourceSpan _) = parentInfo existingItem
                     replacement = x { parentInfo = Just $ Summarizable category name sourceSpan "modified" }
                   in
                     front <> (replacement : back)
                | otherwise = xs <> [x]
        find p x = List.break (p x)
        exactMatch a b = parentInfo a == parentInfo b
        similarMatch a b = case (parentInfo a, parentInfo b) of
          (Just (Summarizable catA nameA _ _), Just (Summarizable catB nameB _ _)) -> catA == catB && toLower nameA == toLower nameB
          (_, _) -> False

    diffToTOCSummaries :: HasDefaultFields fields => Both Source -> Diff (Syntax Text) (Record fields) -> [TOCSummary DiffInfo]
    diffToTOCSummaries sources = para $ \diff -> case diff of
        Free (annotations :< syntax) -> toList diff >>= \ summaries ->
          fmap (contextualize (Both.snd sources) (Both.snd annotations :< fmap fst syntax)) (snd summaries)
        Pure patch -> fmap toTOCSummaries (sequenceA (runBothWith mapPatch (toInfo <$> sources) patch))

    toInfo :: HasDefaultFields fields => Source -> Term (Syntax Text) (Record fields) -> [DiffInfo]
    toInfo source = para $ \ (annotation :< syntax) -> let termName = toTermName source (cofree (annotation :< (fst <$> syntax))) in case syntax of
      S.ParseError _ -> [ErrorInfo (sourceSpan annotation) termName]
      S.Indexed children -> children >>= snd
      S.Fixed children -> children >>= snd
      S.Commented cs leaf -> (cs <> maybeToList leaf) >>= snd
      S.AnonymousFunction _ _ -> [LeafInfo C.AnonymousFunction termName (sourceSpan annotation)]
      _ -> [LeafInfo (category annotation) termName (sourceSpan annotation)]

    toTOCSummaries patch = case afterOrBefore patch of
      ErrorInfo{..} -> TOCSummary patch Nothing
      LeafInfo{..} -> TOCSummary patch $ case leafCategory of
        C.Function -> Just $ Summarizable leafCategory termName leafSourceSpan (patchType patch)
        C.Method -> Just $ Summarizable leafCategory termName leafSourceSpan (patchType patch)
        C.SingletonMethod -> Just $ Summarizable leafCategory termName leafSourceSpan (patchType patch)
        _ -> Nothing

    contextualize source (annotation :< syntax) summary
      | Nothing <- parentInfo summary
      , isSummarizable syntax
      , Just terms <- traverse afterTerm syntax = summary { parentInfo = Just (InSummarizable (category annotation) (toTermName source (cofree (annotation :< terms))) (sourceSpan annotation)) }
      | otherwise = summary

    isSummarizable S.Method{} = True
    isSummarizable S.Function{} = True
    isSummarizable _ = False

toJSONSummaries :: TOCSummary DiffInfo -> [JSONSummary]
toJSONSummaries TOCSummary{..} = toJSONSummaries' (afterOrBefore summaryPatch)
  where
    toJSONSummaries' diffInfo = case diffInfo of
      ErrorInfo{..} -> [ErrorSummary termName infoSpan]
      LeafInfo{..} -> maybe [] (pure . JSONSummary) parentInfo

toTermName :: HasDefaultFields fields => Source -> Term (Syntax Text) (Record fields) -> Text
toTermName source = para $ \ (annotation :< syntax) -> case syntax of
  S.Function (_, identifier) _ _ -> identifier
  S.Method _ (_, identifier) Nothing _ _ -> identifier
  S.Method _ (_, identifier) (Just (receiver, receiverSource)) _ _
    | S.Indexed [receiverParams] <- unwrap receiver
    , S.ParameterDecl (Just ty) _ <- unwrap receiverParams -> "(" <> toTermName source ty <> ") " <> identifier
    | otherwise -> receiverSource <> "." <> identifier
  _ -> toText (Source.slice (byteRange annotation) source)

-- The user-facing category name
toCategoryName :: Category -> Text
toCategoryName category = case category of
  C.SingletonMethod -> "Method"
  c -> show c
