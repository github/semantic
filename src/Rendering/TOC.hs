{-# LANGUAGE DerivingVia, RankNTypes, ScopedTypeVariables, TupleSections #-}
module Rendering.TOC
( diffTOC
, Summaries(..)
, TOCSummary(..)
, isValidSummary
, declaration
, Entry(..)
, tableOfContentsBy
, dedupe
) where

import Prologue hiding (index)
import Analysis.TOCSummary
import Data.Align (bicrosswalk)
import Data.Aeson
import Data.Diff
import Data.Language as Language
import Data.List (sortOn)
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
    { kind       :: T.Text
    , ident      :: T.Text
    , span       :: Span
    , changeType :: T.Text
    }
  | ErrorSummary
    { message  :: T.Text
    , span     :: Span
    , language :: Language
    }
  deriving stock (Eq, Show)

instance ToJSON TOCSummary where
  toJSON TOCSummary{..} = object [ "changeType" .= changeType, "category" .= kind, "term" .= ident, "span" .= span ]
  toJSON ErrorSummary{..} = object [ "error" .= message, "span" .= span, "language" .= language ]

-- | An entry in a table of contents.
data Entry
  = Changed  -- ^ An entry for a node containing changes.
  | Inserted -- ^ An entry for a change occurring inside an 'Insert' 'Patch'.
  | Deleted  -- ^ An entry for a change occurring inside a 'Delete' 'Patch'.
  | Replaced -- ^ An entry for a change occurring on the insertion side of a 'Replace' 'Patch'.
  deriving (Eq, Show)


isValidSummary :: TOCSummary -> Bool
isValidSummary ErrorSummary{} = False
isValidSummary _ = True

-- | Produce the annotations of nodes representing declarations.
declaration :: TermF f (Maybe Declaration) a -> Maybe Declaration
declaration (In annotation _) = annotation

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


data DedupeKey = DedupeKey {-# UNPACK #-} !T.Text {-# UNPACK #-} !T.Text
  deriving (Eq, Ord)

data Dedupe = Dedupe
  { index :: {-# UNPACK #-} !Int
  , entry :: {-# UNPACK #-} !Entry
  , decl  :: {-# UNPACK #-} !Declaration
  }

-- Dedupe entries in a final pass. This catches two specific scenarios with
-- different behaviors:
-- 1. Identical entries are in the list.
--    Action: take the first one, drop all subsequent.
-- 2. Two similar entries (defined by a case insensitive comparison of their
--    identifiers) are in the list.
--    Action: Combine them into a single Replaced entry.
dedupe :: [(Entry, Declaration)] -> [(Entry, Declaration)]
dedupe = map (entry &&& decl) . sortOn index . Map.elems . foldl' go Map.empty . zipWith (uncurry . Dedupe) [0..] where
  go m d@(Dedupe _ _ decl) = let key = dedupeKey decl in case Map.lookup key m of
    Just (Dedupe _ _ similar)
      | similar == decl -> m
      | otherwise       -> Map.insert key d { entry = Replaced, decl = similar } m
    _                   -> Map.insert key d m

  dedupeKey (Declaration kind ident _ _ _) = DedupeKey (formatKind kind) (T.toLower ident)

-- | Construct a description of an 'Entry'.
formatEntry :: Entry -> Text
formatEntry entry = case entry of
  Changed  -> "modified"
  Deleted  -> "removed"
  Inserted -> "added"
  Replaced -> "modified"

-- | Construct a 'TOCSummary' from a node annotation and a change type label.
recordSummary :: Entry -> Declaration -> TOCSummary
recordSummary entry decl@(Declaration kind text _ srcSpan language)
  | ErrorDeclaration <- kind = ErrorSummary text srcSpan language
  | otherwise                = TOCSummary (formatKind kind) (formatIdentifier decl) srcSpan (formatEntry entry)

formatIdentifier :: Declaration -> Text
formatIdentifier (Declaration kind identifier _ _ lang) = case kind of
  MethodDeclaration (Just receiver)
    | Language.Go <- lang -> "(" <> receiver <> ") " <> identifier
    | otherwise           -> receiver <> "." <> identifier
  _                       -> identifier

diffTOC :: (Foldable f, Functor f) => Diff f (Maybe Declaration) (Maybe Declaration) -> [TOCSummary]
diffTOC = map (uncurry recordSummary) . dedupe . tableOfContentsBy declaration

-- The user-facing kind
formatKind :: DeclarationKind -> T.Text
formatKind kind = case kind of
  FunctionDeclaration  -> "Function"
  MethodDeclaration _  -> "Method"
  HeadingDeclaration l -> "Heading " <> T.pack (show l)
  ErrorDeclaration     -> "ParseError"
