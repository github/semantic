{-# LANGUAGE DerivingVia, DuplicateRecordFields, RankNTypes, ScopedTypeVariables, TupleSections #-}
module Rendering.TOC
( diffTOC
, Summaries(..)
, TOCSummary(..)
, ErrorSummary(..)
, declaration
, Change(..)
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

data TOCSummary = TOCSummary
  { kind   :: T.Text
  , ident  :: T.Text
  , span   :: Span
  , change :: Change
  }
  deriving stock (Eq, Show)

data ErrorSummary = ErrorSummary
  { message  :: T.Text
  , span     :: Span
  , language :: Language
  }
  deriving stock (Eq, Show)

instance ToJSON TOCSummary where
  toJSON TOCSummary{..} = object [ "changeType" .= change, "category" .= kind, "term" .= ident, "span" .= span ]

instance ToJSON ErrorSummary where
  toJSON ErrorSummary{..} = object [ "error" .= message, "span" .= span, "language" .= language ]

-- | The kind of a ToC change.
data Change
  = Changed
  | Inserted
  | Deleted
  | Replaced
  deriving (Eq, Show)

instance ToJSON Change where
  toJSON change = case change of
    Changed  -> "modified"
    Deleted  -> "removed"
    Inserted -> "added"
    Replaced -> "modified"


-- | Produce the annotations of nodes representing declarations.
declaration :: TermF f (Maybe Declaration) a -> Maybe Declaration
declaration (In annotation _) = annotation

-- | Compute a table of contents for a diff characterized by a function mapping relevant nodes onto values in Maybe.
tableOfContentsBy :: (Foldable f, Functor f)
                  => (forall b. TermF f ann b -> Maybe a) -- ^ A function mapping relevant nodes onto values in Maybe.
                  -> Diff f ann ann                       -- ^ The diff to compute the table of contents for.
                  -> [(Change, a)]                        -- ^ A list of entries for relevant changed nodes in the diff.
tableOfContentsBy selector = fromMaybe [] . cata (\ r -> case r of
  Patch patch -> (pure . patchEntry <$> bicrosswalk selector selector patch) <> bifoldMap fold fold patch <> Just []
  Merge (In (_, ann2) r) -> case (selector (In ann2 r), fold r) of
    (Just a, Just entries) -> Just ((Changed, a) : entries)
    (_     , entries)      -> entries)
   where patchEntry = patch (Deleted,) (Inserted,) (const (Replaced,))


data DedupeKey = DedupeKey !DeclarationKind {-# UNPACK #-} !T.Text
  deriving (Eq, Ord)

data Dedupe = Dedupe
  { index  :: {-# UNPACK #-} !Int
  , change :: {-# UNPACK #-} !Change
  , decl   :: {-# UNPACK #-} !Declaration
  }

-- Dedupe entries in a final pass. This catches two specific scenarios with
-- different behaviors:
-- 1. Identical entries are in the list.
--    Action: take the first one, drop all subsequent.
-- 2. Two similar entries (defined by a case insensitive comparison of their
--    identifiers) are in the list.
--    Action: Combine them into a single Replaced entry.
dedupe :: [(Change, Declaration)] -> [(Change, Declaration)]
dedupe = map ((change :: Dedupe -> Change) &&& decl) . sortOn index . Map.elems . foldl' go Map.empty . zipWith (uncurry . Dedupe) [0..] where
  go m d@(Dedupe _ _ decl) = let key = dedupeKey decl in case Map.lookup key m of
    Just (Dedupe _ _ similar)
      | similar == decl -> m
      | otherwise       -> Map.insert key d { change = Replaced, decl = similar } m
    _                   -> Map.insert key d m

  dedupeKey (Declaration kind ident _ _ _) = DedupeKey kind (T.toLower ident)

-- | Construct a 'TOCSummary' from a node annotation and a change type label.
recordSummary :: Change -> Declaration -> Either ErrorSummary TOCSummary
recordSummary change decl@(Declaration kind text _ srcSpan language)
  | ErrorDeclaration <- kind = Left  $ ErrorSummary text srcSpan language
  | otherwise                = Right $ TOCSummary (formatKind kind) (formatIdentifier decl) srcSpan change

formatIdentifier :: Declaration -> Text
formatIdentifier (Declaration kind identifier _ _ lang) = case kind of
  MethodDeclaration (Just receiver)
    | Language.Go <- lang -> "(" <> receiver <> ") " <> identifier
    | otherwise           -> receiver <> "." <> identifier
  _                       -> identifier

diffTOC :: (Foldable f, Functor f) => Diff f (Maybe Declaration) (Maybe Declaration) -> [Either ErrorSummary TOCSummary]
diffTOC = map (uncurry recordSummary) . dedupe . tableOfContentsBy declaration

-- The user-facing kind
formatKind :: DeclarationKind -> T.Text
formatKind kind = case kind of
  FunctionDeclaration  -> "Function"
  MethodDeclaration _  -> "Method"
  HeadingDeclaration l -> "Heading " <> T.pack (show l)
  ErrorDeclaration     -> "ParseError"
