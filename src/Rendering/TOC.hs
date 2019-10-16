{-# LANGUAGE DerivingVia, RankNTypes, ScopedTypeVariables, TupleSections #-}
module Rendering.TOC
( renderToCDiff
, diffTOC
, Summaries(..)
, TOCSummary(..)
, isValidSummary
, declaration
, Entry(..)
, tableOfContentsBy
, termTableOfContentsBy
, dedupe
, toCategoryName
) where

import Prologue
import Analysis.TOCSummary
import Data.Align (bicrosswalk)
import Data.Aeson
import Data.Blob
import Data.Diff
import Data.Language as Language
import Data.List (sortOn)
import qualified Data.List as List
import qualified Data.Map.Monoidal as Map
import Data.Patch
import Data.Term
import qualified Data.Text as T
import Source.Loc

data Summaries = Summaries { changes, errors :: Map.Map T.Text [Value] }
  deriving stock (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup Summaries
  deriving Monoid via GenericMonoid Summaries

instance ToJSON Summaries where
  toJSON Summaries{..} = object [ "changes" .= changes, "errors" .= errors ]

data TOCSummary
  = TOCSummary
    { summaryCategoryName :: T.Text
    , summaryTermName :: T.Text
    , summarySpan :: Span
    , summaryChangeType :: T.Text
    }
  | ErrorSummary { errorText :: T.Text, errorSpan :: Span, errorLanguage :: Language }
  deriving stock (Generic, Eq, Show)

instance ToJSON TOCSummary where
  toJSON TOCSummary{..} = object [ "changeType" .= summaryChangeType, "category" .= summaryCategoryName, "term" .= summaryTermName, "span" .= summarySpan ]
  toJSON ErrorSummary{..} = object [ "error" .= errorText, "span" .= errorSpan, "language" .= errorLanguage ]

isValidSummary :: TOCSummary -> Bool
isValidSummary ErrorSummary{} = False
isValidSummary _ = True

-- | Produce the annotations of nodes representing declarations.
declaration :: TermF f (Maybe Declaration) a -> Maybe Declaration
declaration (In annotation _) = annotation


-- | An entry in a table of contents.
data Entry
  = Changed  -- ^ An entry for a node containing changes.
  | Inserted -- ^ An entry for a change occurring inside an 'Insert' 'Patch'.
  | Deleted  -- ^ An entry for a change occurring inside a 'Delete' 'Patch'.
  | Replaced -- ^ An entry for a change occurring on the insertion side of a 'Replace' 'Patch'.
  deriving (Eq, Show)


-- | Compute a table of contents for a diff characterized by a function mapping relevant nodes onto values in Maybe.
tableOfContentsBy :: (Foldable f, Functor f)
                  => (forall b. TermF f ann b -> Maybe a) -- ^ A function mapping relevant nodes onto values in Maybe.
                  -> Diff f ann ann                       -- ^ The diff to compute the table of contents for.
                  -> [(Entry, a)]                         -- ^ A list of entries for relevant changed nodes in the diff.
tableOfContentsBy selector = fromMaybe [] . cata (\ r -> case r of
  Patch patch -> (pure . patchEntry <$> bicrosswalk selector selector patch) <> bifoldMap fold fold patch <> Just []
  Merge (In (_, ann2) r) -> case (selector (In ann2 r), fold r) of
    (Just a, Just entries) -> Just ((Changed, a) : entries)
    (_     , entries)      -> entries)
   where patchEntry = patch (Deleted,) (Inserted,) (const (Replaced,))

termTableOfContentsBy :: (Foldable f, Functor f)
                      => (forall b. TermF f annotation b -> Maybe a)
                      -> Term f annotation
                      -> [a]
termTableOfContentsBy selector = cata termAlgebra
  where termAlgebra r | Just a <- selector r = a : fold r
                      | otherwise = fold r

newtype DedupeKey = DedupeKey (T.Text, T.Text) deriving (Eq, Ord)

-- Dedupe entries in a final pass. This catches two specific scenarios with
-- different behaviors:
-- 1. Identical entries are in the list.
--    Action: take the first one, drop all subsequent.
-- 2. Two similar entries (defined by a case insensitive comparison of their
--    identifiers) are in the list.
--    Action: Combine them into a single Replaced entry.
dedupe :: [(Entry, Declaration)] -> [(Entry, Declaration)]
dedupe = map snd . sortOn fst . Map.elems . snd . foldl' (uncurry . go) (0, Map.empty)
  where
    go :: (Int, Map.Map DedupeKey (Int, (Entry, Declaration)))
       -> Entry
       -> Declaration
       -> (Int, Map.Map DedupeKey (Int, (Entry, Declaration)))
    go (index, m) entry decl
      | Just (_, (_, similarDecl)) <- Map.lookup (dedupeKey decl) m
      = (succ index,) $ if similarDecl == decl then
          m
        else
          Map.insert (dedupeKey similarDecl) (index, (Replaced, similarDecl)) m
      | otherwise = (succ index, Map.insert (dedupeKey decl) (index, (entry, decl)) m)

    dedupeKey (Declaration kind ident _ _ _) = DedupeKey (toCategoryName kind, T.toLower ident)

-- | Construct a description of an 'Entry'.
entryChange :: Entry -> Text
entryChange entry = case entry of
  Changed  -> "modified"
  Deleted  -> "removed"
  Inserted -> "added"
  Replaced -> "modified"

-- | Construct a 'TOCSummary' from a node annotation and a change type label.
recordSummary :: T.Text -> Declaration -> TOCSummary
recordSummary changeText record = case record of
  Declaration ErrorDeclaration text _ srcSpan language -> ErrorSummary text srcSpan language
  decl -> TOCSummary (toCategoryName (declarationKind decl)) (formatIdentifier decl) (declarationSpan decl) changeText
  where
    formatIdentifier (Declaration kind identifier _ _ lang) = case kind of
      MethodDeclaration (Just receiver)
        | Language.Go <- lang -> "(" <> receiver <> ") " <> identifier
        | otherwise           -> receiver <> "." <> identifier
      _                       -> identifier

renderToCDiff :: (Foldable f, Functor f) => BlobPair -> Diff f (Maybe Declaration) (Maybe Declaration) -> Summaries
renderToCDiff blobs = uncurry Summaries . bimap toMap toMap . List.partition isValidSummary . diffTOC
  where toMap [] = mempty
        toMap as = Map.singleton summaryKey (toJSON <$> as)
        summaryKey = T.pack $ pathKeyForBlobPair blobs

diffTOC :: (Foldable f, Functor f) => Diff f (Maybe Declaration) (Maybe Declaration) -> [TOCSummary]
diffTOC = map (uncurry (recordSummary . entryChange)) . dedupe . tableOfContentsBy declaration

-- The user-facing category name
toCategoryName :: DeclarationKind -> T.Text
toCategoryName kind = case kind of
  FunctionDeclaration  -> "Function"
  MethodDeclaration _  -> "Method"
  HeadingDeclaration l -> "Heading " <> T.pack (show l)
  ErrorDeclaration     -> "ParseError"
